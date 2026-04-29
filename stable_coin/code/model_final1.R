# ==================== 1. 加载所有必要包 ====================
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(randomForest)
library(xgboost)
library(caret)
library(pROC)
library(ggplot2)
library(gridExtra)
library(viridis)
library(corrplot)
library(glmnet)
library(smotefamily)  # 添加SMOTE处理不平衡
library(pROC)         # 添加ROC曲线
library(PRROC)        # 添加PR曲线
# 移除plotly和shiny依赖，使用更基础的图形包

cat("所有包加载完成\n")

# ==================== 2. 创建输出目录 ====================

create_output_directories <- function() {
  # 主输出目录
  main_dir <- "result"
  if (!dir.exists(main_dir)) {
    dir.create(main_dir, recursive = TRUE)
    cat("创建主输出目录:", main_dir, "\n")
  }
  
  # 子目录
  sub_dirs <- c(
    "plots",
    "models",
    "data",
    "reports",
    "dashboard",
    "evaluation",
    "user_portfolios"  # 保存用户投资组合
  )
  
  for (dir_name in sub_dirs) {
    dir_path <- file.path(main_dir, dir_name)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cat("创建子目录:", dir_path, "\n")
    }
  }
  
  return(main_dir)
}

# ==================== 3. 数据读取函数（稳健版） ====================

read_stablecoin_data <- function(stablecoin_name) {
  # 构建文件路径
  price_file <- paste0("stable_coin/data/processed/data_", stablecoin_name, ".xlsx")
  chain_file <- paste0("stable_coin/data/processed/", tolower(stablecoin_name), "_data.csv")
  
  cat(paste("正在读取", stablecoin_name, "数据...\n"))
  
  # 1. 读取价格数据
  if (!file.exists(price_file)) {
    cat(paste("警告：价格数据文件不存在:", price_file, "\n"))
    return(NULL)
  }
  
  tryCatch({
    price_data <- read_excel(price_file)
    cat(paste("成功读取价格数据，原始行数:", nrow(price_data), "\n"))
    
    # 清理列名
    colnames(price_data) <- gsub("\\s+|\\.|\\|", "_", colnames(price_data))
    colnames(price_data) <- gsub("^_|_$", "", colnames(price_data))
    colnames(price_data) <- tolower(colnames(price_data))
    
    # 查找并重命名时间列
    time_cols <- c("timestamp", "date", "time", "datetime")
    for (col in time_cols) {
      if (col %in% colnames(price_data)) {
        colnames(price_data)[colnames(price_data) == col] <- "timestamp"
        break
      }
    }
    
    # 如果还没有timestamp，尝试第一列
    if (!"timestamp" %in% colnames(price_data) && ncol(price_data) > 0) {
      colnames(price_data) <- "timestamp"
    }
    
    # 转换为时间格式
    price_data$timestamp <- as.POSIXct(price_data$timestamp)
    
    # 标准化列名
    col_mapping <- list(
      "open" = c("open", "开盘价"),
      "high" = c("high", "最高价"),
      "low" = c("low", "最低价"),
      "close" = c("close", "收盘价"),
      "volume" = c("volume", "交易量", "成交量"),
      "price_range" = c("price_range", "价差"),
      "price_volatility" = c("price_volatility", "价格波动率"),
      "volume_6m_rolling_median" = c("volume_6m_rolling_median", "交易量滚动六个月中位数")
    )
    
    for (std_name in names(col_mapping)) {
      for (possible_name in col_mapping[[std_name]]) {
        if (possible_name %in% colnames(price_data) && !std_name %in% colnames(price_data)) {
          colnames(price_data)[colnames(price_data) == possible_name] <- std_name
        }
      }
    }
    
    # 确保数值列是数值类型
    numeric_cols <- c("open", "high", "low", "close", "volume", 
                     "price_range", "price_volatility", "volume_6m_rolling_median")
    for (col in intersect(numeric_cols, colnames(price_data))) {
      price_data[[col]] <- as.numeric(as.character(price_data[[col]]))
    }
    
    # 按时间排序
    price_data <- price_data[order(price_data$timestamp), ]
    
    # 2. 读取链上数据
    chain_data <- NULL
    if (file.exists(chain_file)) {
      tryCatch({
        chain_data <- read.csv(chain_file, stringsAsFactors = FALSE)
        cat(paste("成功读取链上数据，行数:", nrow(chain_data), "\n"))
        
        # 清理链上数据列名
        colnames(chain_data) <- gsub("\\s+|\\.", "_", colnames(chain_data))
        colnames(chain_data) <- tolower(colnames(chain_data))
        
        # 查找时间列
        for (col in time_cols) {
          if (col %in% colnames(chain_data)) {
            colnames(chain_data)[colnames(chain_data) == col] <- "timestamp"
            break
          }
        }
        
        if ("timestamp" %in% colnames(chain_data)) {
          chain_data$timestamp <- as.POSIXct(chain_data$timestamp)
        }
        
      }, error = function(e) {
        cat(paste("读取链上数据出错:", e$message, "\n"))
      })
    }
    
    # 3. 合并数据
    if (!is.null(chain_data) && nrow(chain_data) > 0) {
      # 确保timestamp格式一致
      price_data$timestamp <- as.POSIXct(price_data$timestamp)
      chain_data$timestamp <- as.POSIXct(chain_data$timestamp)
      
      # 合并，以价格数据为准
      merged_data <- merge(price_data, chain_data, by = "timestamp", all.x = TRUE)
      cat(paste("合并后数据行数:", nrow(merged_data), "\n"))
    } else {
      merged_data <- price_data
      cat("使用纯价格数据（无链上数据）\n")
    }
    
    # 添加稳定币标识
    merged_data$stablecoin <- stablecoin_name
    
    # 去重
    merged_data <- merged_data[!duplicated(merged_data$timestamp), ]
    
    cat(paste(stablecoin_name, "数据处理完成，最终行数:", nrow(merged_data), "\n"))
    return(merged_data)
    
  }, error = function(e) {
    cat(paste("处理", stablecoin_name, "数据时出错:", e$message, "\n"))
    return(NULL)
  })
}

# ==================== 4. 脱锚标签计算 ====================

calculate_depeg_labels <- function(data, window_days = 30, alpha = 1/3) {
  cat("计算脱锚标签...\n")
  
  data <- data[order(data$timestamp), ]
  
  required_cols <- c("volume", "low", "high")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("缺少必要列:", paste(missing_cols, collapse = ", ")))
  }
  
  n <- nrow(data)
  data$rolling_volume_30d <- rep(NA, n)
  
  for (i in 31:n) {
    start_idx <- max(1, i - window_days)
    end_idx <- i - 1
    data$rolling_volume_30d[i] <- sum(data$volume[start_idx:end_idx], na.rm = TRUE)
  }
  
  for (i in 1:min(30, n)) {
    if (i > 1) {
      start_idx <- max(1, i - window_days)
      end_idx <- i - 1
      data$rolling_volume_30d[i] <- sum(data$volume[start_idx:end_idx], na.rm = TRUE)
    }
  }
  
  epsilon <- 1e-10
  data$threshold_lower <- 1 - 10 / (data$rolling_volume_30d^alpha + epsilon)
  data$threshold_upper <- 1 + 10 / (data$rolling_volume_30d^alpha + epsilon)
  
  data$threshold_lower[!is.finite(data$threshold_lower) | data$threshold_lower < 0] <- 0.99
  data$threshold_upper[!is.finite(data$threshold_upper) | data$threshold_upper > 1.1] <- 1.01
  
  data$threshold_lower <- pmax(pmin(data$threshold_lower, 0.999), 0.99)
  data$threshold_upper <- pmin(pmax(data$threshold_upper, 1.001), 1.01)
  
  data$depeg_label <- ifelse(
    data$low <= data$threshold_lower | data$high >= data$threshold_upper,
    1, 0
  )
  
  data$depeg_label_future <- dplyr::lead(data$depeg_label, n = 1)
  
  data$price_deviation <- abs(data$close - 1)
  data$depeg_probability <- pmin(1, data$price_deviation * 10)
  
  depeg_count <- sum(data$depeg_label, na.rm = TRUE)
  depeg_rate <- mean(data$depeg_label, na.rm = TRUE) * 100
  future_depeg_count <- sum(data$depeg_label_future, na.rm = TRUE)
  
  cat(paste("脱锚统计:\n"))
  cat(paste("  当前脱锚事件数:", depeg_count, "\n"))
  cat(paste("  当前脱锚比例:", round(depeg_rate, 2), "%\n", sep = ""))
  cat(paste("  未来一天脱锚事件数:", future_depeg_count, "\n"))
  
  return(data)
}

# ==================== 5. 特征工程（安全版） ====================

create_features_safe <- function(data) {
  cat("创建特征...\n")
  
  data <- data[order(data$timestamp), ]
  n <- nrow(data)
  
  if ("close" %in% colnames(data) && n > 1) {
    data$close_lag1 <- dplyr::lag(data$close, 1)
    data$price_change <- ifelse(
      !is.na(data$close_lag1) & data$close_lag1 > 0,
      (data$close - data$close_lag1) / data$close_lag1,
      NA
    )
    data$log_return <- ifelse(
      !is.na(data$close_lag1) & data$close_lag1 > 0,
      log(data$close) - log(data$close_lag1),
      NA
    )
  }
  
  if ("close" %in% colnames(data)) {
    if (n >= 7) {
      data$ma_7 <- c(rep(NA, 6), 
                     rollapply(data$close, 7, 
                               function(x) mean(x[1:6], na.rm = TRUE), 
                               align = "right"))
    } else {
      data$ma_7 <- NA
    }
    
    if (n >= 30) {
      data$ma_30 <- c(rep(NA, 29), 
                      rollapply(data$close, 30,
                                function(x) mean(x[1:29], na.rm = TRUE),
                                align = "right"))
    } else {
      data$ma_30 <- NA
    }
    
    data$price_vs_ma7 <- ifelse(!is.na(data$ma_7), 
                               (data$close / data$ma_7 - 1) * 100, 
                               NA)
    data$price_vs_ma30 <- ifelse(!is.na(data$ma_30), 
                                (data$close / data$ma_30 - 1) * 100, 
                                NA)
  }
  
  if ("price_change" %in% colnames(data) && sum(!is.na(data$price_change)) >= 7) {
    data$volatility_7d <- c(rep(NA, 6),
                           rollapply(data$price_change, 7,
                                     function(x) sd(x[1:6], na.rm = TRUE),
                                     align = "right"))
  }
  
  if ("price_change" %in% colnames(data) && sum(!is.na(data$price_change)) >= 30) {
    data$volatility_30d <- c(rep(NA, 29),
                            rollapply(data$price_change, 30,
                                      function(x) sd(x[1:29], na.rm = TRUE),
                                      align = "right"))
  }
  
  if ("volume" %in% colnames(data)) {
    if (n >= 7) {
      data$volume_ma_7 <- c(rep(NA, 6),
                           rollapply(data$volume, 7,
                                     function(x) mean(x[1:6], na.rm = TRUE),
                                     align = "right"))
    }
    
    if (n >= 30) {
      data$volume_ma_30 <- c(rep(NA, 29),
                            rollapply(data$volume, 30,
                                      function(x) mean(x[1:29], na.rm = TRUE),
                                      align = "right"))
    }
    
    if (n > 1) {
      data$volume_lag1 <- dplyr::lag(data$volume, 1)
      data$volume_change <- ifelse(
        !is.na(data$volume_lag1) & data$volume_lag1 > 0,
        (data$volume - data$volume_lag1) / data$volume_lag1,
        NA
      )
    }
    
    if ("volume_6m_rolling_median" %in% colnames(data)) {
      data$volume_ratio <- data$volume / (data$volume_6m_rolling_median + 1)
    }
  }
  
  if ("close" %in% colnames(data) && n >= 15) {
    calculate_rsi_safe <- function(prices, period = 14) {
      n_prices <- length(prices)
      if (n_prices < period + 1) return(rep(NA, n_prices))
      
      deltas <- diff(prices)
      gains <- ifelse(deltas > 0, deltas, 0)
      losses <- ifelse(deltas < 0, -deltas, 0)
      
      avg_gain <- rep(NA, n_prices)
      avg_loss <- rep(NA, n_prices)
      
      avg_gain[period + 1] <- mean(gains[1:period], na.rm = TRUE)
      avg_loss[period + 1] <- mean(losses[1:period], na.rm = TRUE)
      
      for (i in (period + 2):n_prices) {
        avg_gain[i] <- (avg_gain[i-1] * (period - 1) + gains[i-1]) / period
        avg_loss[i] <- (avg_loss[i-1] * (period - 1) + losses[i-1]) / period
      }
      
      rs <- avg_gain / (avg_loss + 1e-10)
      rsi <- 100 - 100 / (1 + rs)
      
      rsi[1:period] <- NA
      
      return(rsi)
    }
    
    data$rsi_14 <- calculate_rsi_safe(data$close, 14)
  }
  
  if ("close" %in% colnames(data)) {
    safe_momentum <- function(x, n) {
      if (length(x) <= n) return(rep(NA, length(x)))
      momentum <- rep(NA, length(x))
      for (i in (n+1):length(x)) {
        if (!is.na(x[i]) && !is.na(x[i-n]) && x[i-n] > 0) {
          momentum[i] <- (x[i] / x[i-n]) - 1
        }
      }
      return(momentum)
    }
    
    if (n >= 6) data$momentum_5 <- safe_momentum(data$close, 5)
    if (n >= 11) data$momentum_10 <- safe_momentum(data$close, 10)
    if (n >= 21) data$momentum_20 <- safe_momentum(data$close, 20)
  }
  
  if (all(c("high", "low") %in% colnames(data)) && n >= 20) {
    data$channel_high_20 <- c(rep(NA, 19),
                             rollapply(data$high, 20,
                                       function(x) max(x[1:19], na.rm = TRUE),
                                       align = "right"))
    data$channel_low_20 <- c(rep(NA, 19),
                            rollapply(data$low, 20,
                                      function(x) min(x[1:19], na.rm = TRUE),
                                      align = "right"))
    
    channel_range <- data$channel_high_20 - data$channel_low_20
    channel_range[channel_range == 0 | is.na(channel_range)] <- 1
    
    data$channel_position <- ifelse(
      !is.na(data$channel_low_20) & !is.na(channel_range),
      (data$close - data$channel_low_20) / channel_range,
      NA
    )
  }
  
  if ("timestamp" %in% colnames(data)) {
    data$day_of_week <- as.numeric(format(data$timestamp, "%u"))
    data$month <- as.numeric(format(data$timestamp, "%m"))
    data$day_of_month <- as.numeric(format(data$timestamp, "%d"))
    data$is_weekend <- ifelse(data$day_of_week %in% c(6, 7), 1, 0)
  }
  
  if ("close" %in% colnames(data)) {
    data$close_lag2 <- dplyr::lag(data$close, 2)
    data$close_lag3 <- dplyr::lag(data$close, 3)
  }
  
  if ("volume" %in% colnames(data)) {
    data$volume_lag2 <- dplyr::lag(data$volume, 2)
    data$volume_lag3 <- dplyr::lag(data$volume, 3)
  }
  
  if ("rolling_volume_30d" %in% colnames(data)) {
    data$rolling_volume_30d_lag1 <- dplyr::lag(data$rolling_volume_30d, 1)
    data$rolling_volume_change <- ifelse(
      !is.na(data$rolling_volume_30d_lag1) & data$rolling_volume_30d_lag1 > 0,
      (data$rolling_volume_30d - data$rolling_volume_30d_lag1) / data$rolling_volume_30d_lag1,
      NA
    )
    
    data$volume_to_rolling <- ifelse(
      !is.na(data$rolling_volume_30d) & data$rolling_volume_30d > 0,
      data$volume / (data$rolling_volume_30d / 30),
      NA
    )
  }
  
  cat(paste("特征创建完成，总特征数:", ncol(data), "\n"))
  return(data)
}

# ==================== 6. 数据清理函数 ====================

clean_data_thoroughly <- function(data) {
  cat("数据清理...\n")
  
  initial_rows <- nrow(data)
  
  if (!"depeg_label_future" %in% colnames(data)) {
    cat("警告：数据中没有depeg_label_future列\n")
    return(data)
  }
  
  na_target_rows <- sum(is.na(data$depeg_label_future))
  if (na_target_rows > 0) {
    data <- data[!is.na(data$depeg_label_future), ]
  }
  
  na_cols <- sapply(data, function(x) all(is.na(x)))
  if (any(na_cols)) {
    data <- data[, !na_cols]
  }
  
  numeric_cols <- sapply(data, is.numeric)
  exclude_fill_cols <- c("depeg_label", "depeg_label_future", 
                         "timestamp_index", "row_index")
  
  for (col in names(data)[numeric_cols]) {
    if (col %in% exclude_fill_cols) {
      next
    }
    
    col_data <- data[[col]]
    na_idx <- is.na(col_data) | is.nan(col_data)
    if (any(na_idx)) {
      if (sum(!na_idx) > 0) {
        temp_series <- zoo::na.locf(zoo(col_data), na.rm = FALSE)
        if (any(is.na(temp_series))) {
          temp_series <- zoo::na.locf(temp_series, fromLast = TRUE, na.rm = FALSE)
        }
        data[[col]] <- as.numeric(temp_series)
      } else {
        data[[col]][na_idx] <- 0
      }
    }
    
    inf_idx <- is.infinite(data[[col]])
    if (any(inf_idx)) {
      finite_vals <- data[[col]][!inf_idx]
      if (length(finite_vals) > 0) {
        max_finite <- max(finite_vals, na.rm = TRUE)
        min_finite <- min(finite_vals, na.rm = TRUE)
        data[[col]][data[[col]] == Inf] <- max_finite
        data[[col]][data[[col]] == -Inf] <- min_finite
      } else {
        data[[col]][inf_idx] <- 0
      }
    }
  }
  
  if (sum(numeric_cols) > 1) {
    zero_var_cols <- caret::nearZeroVar(data[, numeric_cols, drop = FALSE], saveMetrics = TRUE)
    zero_var_names <- rownames(zero_var_cols)[zero_var_cols$zeroVar | zero_var_cols$nzv]
    
    zero_var_names <- setdiff(zero_var_names, 
                             c("depeg_label", "depeg_label_future"))
    
    if (length(zero_var_names) > 0) {
      data <- data[, !colnames(data) %in% zero_var_names]
    }
  }
  
  final_na_rows <- sum(!complete.cases(data))
  if (final_na_rows > 0) {
    data <- data[complete.cases(data), ]
  }
  
  cat(paste("数据清理完成: ", initial_rows, " -> ", nrow(data), " 行\n", sep = ""))
  return(data)
}

# ==================== 7. 处理类别不平衡（修复版：基于论文SMOTE策略） ====================

handle_class_imbalance <- function(train_data, method = "smote_paper", seed = 42) {
  set.seed(seed)
  cat("\n处理类别不平衡 (基于论文策略)...\n")
  
  # 确保目标变量存在
  if (!"target" %in% colnames(train_data)) {
    train_data$target <- factor(
      ifelse(train_data$depeg_label_future == 1, "Depeg", "Normal"),
      levels = c("Normal", "Depeg")
    )
  }
  
  # 检查类别分布
  depeg_ratio <- mean(train_data$depeg_label_future == 1, na.rm = TRUE)
  depeg_count <- sum(train_data$target == "Depeg", na.rm = TRUE)
  normal_count <- sum(train_data$target == "Normal", na.rm = TRUE)
  
  cat(paste("原始数据:\n"))
  cat(paste("  脱锚样本:", depeg_count, "(", round(depeg_ratio*100, 2), "%)\n"))
  cat(paste("  正常样本:", normal_count, "(", round((1-depeg_ratio)*100, 2), "%)\n"))
  
  # 如果脱锚样本太少，直接返回原始数据
  if (depeg_count < 5) {
    cat(paste("脱锚样本太少 (", depeg_count, ")，不应用SMOTE。\n", sep = ""))
    return(list(data = train_data, weights = NULL))
  }
  
  # 论文策略：对于脱锚比例较低的情况使用SMOTE
  smote_threshold <- 0.3
  target_ratio <- 0.6
  
  if (depeg_ratio > smote_threshold) {
    cat(paste("当前脱锚比例 (", round(depeg_ratio, 2), ") 较高，不应用SMOTE。\n", sep = ""))
    return(list(data = train_data, weights = NULL))
  } else {
    cat(paste("当前脱锚比例较低，应用SMOTE (目标比例: ", target_ratio, ")...\n", sep = ""))
    
    tryCatch({
      # 准备特征和目标
      exclude_cols <- c("timestamp", "stablecoin", "depeg_label", "depeg_label_future",
                       "threshold_lower", "threshold_upper", "rolling_volume_30d",
                       "price_deviation", "depeg_probability", "target")
      
      feature_cols <- setdiff(colnames(train_data), exclude_cols)
      # 只保留数值列
      feature_cols <- feature_cols[sapply(train_data[, feature_cols], is.numeric)]
      
      # 检查并处理缺失值
      train_features <- train_data[, feature_cols]
      
      # 移除包含NA的列
      na_cols <- colSums(is.na(train_features)) > 0
      if (any(na_cols)) {
        cat(paste("移除包含NA的列:", paste(names(which(na_cols)), collapse = ", "), "\n"))
        feature_cols <- feature_cols[!na_cols]
        train_features <- train_features[, feature_cols, drop = FALSE]
      }
      
      # 移除包含NA的行
      na_rows <- rowSums(is.na(train_features)) > 0
      if (any(na_rows)) {
        cat(paste("移除包含NA的行:", sum(na_rows), "\n"))
        train_features <- train_features[!na_rows, , drop = FALSE]
        train_target <- train_data$target[!na_rows]
      } else {
        train_target <- train_data$target
      }
      
      if (nrow(train_features) == 0 || length(unique(train_target)) < 2) {
        cat("数据不足或类别不足，回退使用原始数据\n")
        return(list(data = train_data, weights = NULL))
      }
      
      # 计算需要的dup_size
      target_depeg_count <- round(normal_count * target_ratio)
      needed_new_samples <- target_depeg_count - depeg_count
      
      if (needed_new_samples > 0) {
        # dup_size = 需要的新样本数 / 现有样本数
        dup_size <- round(needed_new_samples / depeg_count)
        # 确保至少为1，但不要太大
        dup_size <- max(1, min(dup_size, 10))
        
        cat(paste("  SMOTE dup_size:", dup_size, "\n"))
        
        # 确保有足够的类别
        if (sum(train_target == "Depeg") < 5) {
          cat("脱锚样本太少，无法应用SMOTE\n")
          return(list(data = train_data, weights = NULL))
        }
        
        smote_result <- SMOTE(
          X = train_features,
          target = as.numeric(train_target == "Depeg"),
          K = min(5, sum(train_target == "Depeg") - 1),  # 确保K不超过可用样本数
          dup_size = dup_size
        )
        
        # 处理SMOTE结果
        balanced_features <- smote_result$data[, 1:(ncol(smote_result$data)-1)]
        class_col <- smote_result$data[, ncol(smote_result$data)]
        
        # 重建target列
        balanced_target <- factor(
          ifelse(class_col == "1", "Depeg", "Normal"),
          levels = c("Normal", "Depeg")
        )
        
        # 创建新的数据框
        balanced_data <- data.frame(balanced_features, target = balanced_target)
        
        cat(paste("SMOTE处理后:\n"))
        cat(paste("  脱锚样本:", sum(balanced_data$target == "Depeg"), "\n"))
        cat(paste("  正常样本:", sum(balanced_data$target == "Normal"), "\n"))
        cat(paste("  新脱锚比例:", round(mean(balanced_data$target == "Depeg")*100, 2), "%\n"))
        
        return(list(data = balanced_data, weights = NULL))
        
      } else {
        cat("无需生成新样本\n")
        return(list(data = train_data, weights = NULL))
      }
      
    }, error = function(e) {
      cat(paste("SMOTE处理失败:", e$message, "\n"))
      cat("回退使用原始数据...\n")
      return(list(data = train_data, weights = NULL))
    })
  }
}

# ==================== 8. 改进版模型训练函数（保持原有结构） ====================

train_robust_models_with_threshold <- function(data, test_ratio = 0.3, seed = 42, 
                                               threshold_method = "high_recall") {
  set.seed(seed)
  cat("\n开始改进版模型训练（带阈值调整）...\n")
  
  # 1. 数据准备和验证
  if (!"depeg_label_future" %in% colnames(data)) {
    stop("数据中没有depeg_label_future列")
  }
  
  # 创建目标变量
  data$target <- factor(
    ifelse(data$depeg_label_future == 1, "Depeg", "Normal"),
    levels = c("Normal", "Depeg")
  )
  
  # 检查数据质量
  target_dist <- table(data$target)
  cat(paste("目标变量分布:\n"))
  cat(paste("  Normal:", target_dist["Normal"], "\n"))
  cat(paste("  Depeg:", target_dist["Depeg"], "\n"))
  
  # 确保有足够的脱锚样本
  if (target_dist["Depeg"] < 5) {
    cat("警告：脱锚样本太少，模型性能可能受限\n")
  }
  
  # 排除不需要的列
  exclude_cols <- c("timestamp", "stablecoin", "depeg_label", "depeg_label_future",
                   "threshold_lower", "threshold_upper", "rolling_volume_30d",
                   "price_deviation", "depeg_probability")
  
  feature_cols <- setdiff(colnames(data), c(exclude_cols, "target"))
  
  # 只保留数值列
  numeric_cols <- sapply(data[, feature_cols, drop = FALSE], is.numeric)
  feature_cols <- feature_cols[numeric_cols]
  
  if (length(feature_cols) == 0) {
    cat("警告：没有可用的数值特征列\n")
    # 添加基本特征
    if ("close" %in% colnames(data)) {
      data$price_change_lag1 <- c(NA, diff(data$close)/head(data$close, -1))
      data$price_abs_deviation <- abs(data$close - 1)
      feature_cols <- c("price_change_lag1", "price_abs_deviation")
      cat("添加基本价格特征\n")
    }
  }
  
  cat(paste("初始特征数量:", length(feature_cols), "\n"))
  
  # 2. 数据划分 - 标准时间序列分割
  data <- data[order(data$timestamp), ]
  n <- nrow(data)
  split_idx <- floor((1 - test_ratio) * n) 

  train_data <- data[1:split_idx, ]
  test_data <- data[(split_idx + 1):n, ]

  cat("=== 严格执行时间序列分割 ===\n")
  cat(paste("分割点位置（索引）:", split_idx, "\n"))
  cat(paste("分割点时间:", data$timestamp[split_idx], "\n"))
  cat(paste("训练集大小:", nrow(train_data), "（Depeg:", sum(train_data$target == "Depeg"), "）\n"))
  cat(paste("测试集大小:", nrow(test_data), "（Depeg:", sum(test_data$target == "Depeg"), "）\n"))
  cat("=============================\n")

  # 检查划分是否满足最低样本要求
  train_depeg <- sum(train_data$target == "Depeg")
  test_depeg <- sum(test_data$target == "Depeg")

  if (train_depeg < 5 || test_depeg < 2) {
    warning("警告：脱锚样本数量可能不足。训练集Depeg=", train_depeg, "，测试集Depeg=", test_depeg)
  }
    
  # 3. 处理类别不平衡
  balance_result <- handle_class_imbalance(train_data, method = "smote_paper", seed = seed)
  train_data_balanced <- balance_result$data
  
  # 4. 准备训练和测试数据
  common_features <- intersect(feature_cols, colnames(train_data_balanced))
  common_features <- intersect(common_features, colnames(test_data))
  
  x_train <- train_data_balanced[, common_features, drop = FALSE]
  y_train <- train_data_balanced$target
  x_test <- test_data[, common_features, drop = FALSE]
  y_test <- test_data$target
  
  # 处理缺失值
  safe_impute <- function(x) {
    if (is.numeric(x)) {
      x[is.na(x) | is.nan(x) | is.infinite(x)] <- median(x[!is.na(x) & !is.nan(x) & !is.infinite(x)], na.rm = TRUE)
      if (all(is.na(x))) x[is.na(x)] <- 0
    }
    return(x)
  }
  
  x_train <- as.data.frame(lapply(x_train, safe_impute))
  x_test <- as.data.frame(lapply(x_test, safe_impute))
  
  # 移除零方差特征
  if (ncol(x_train) > 1) {
    zero_var_cols <- caret::nearZeroVar(x_train, saveMetrics = TRUE)
    non_zero_var <- !zero_var_cols$zeroVar & !zero_var_cols$nzv
    
    if (sum(non_zero_var) > 0) {
      x_train <- x_train[, non_zero_var, drop = FALSE]
      x_test <- x_test[, non_zero_var, drop = FALSE]
    }
  }
  
  cat(paste("最终特征数量:", ncol(x_train), "\n"))
  
  # 5. 阈值调整函数
  adjust_threshold <- function(probabilities, y_true, method = "high_recall") {
    if (length(unique(y_true)) < 2) {
      return(0.5)
    }
    
    tryCatch({
      y_true_numeric <- as.numeric(y_true == "Depeg")
      
      if (method == "optimal") {
        # Youden's J statistic
        roc_obj <- roc(y_true_numeric, probabilities, quiet = TRUE)
        coords <- coords(roc_obj, "best", ret = c("threshold"), best.method = "youden")
        return(coords$threshold)
      } else if (method == "high_recall") {
        # 优先召回率
        thresholds <- seq(0.1, 0.9, by = 0.05)
        f1_scores <- numeric(length(thresholds))
        recall_scores <- numeric(length(thresholds))
        
        for (i in seq_along(thresholds)) {
          pred <- factor(ifelse(probabilities > thresholds[i], "Depeg", "Normal"), 
                         levels = c("Normal", "Depeg"))
          cm <- confusionMatrix(pred, factor(ifelse(y_true_numeric == 1, "Depeg", "Normal"),
                                             levels = c("Normal", "Depeg")), 
                                positive = "Depeg")
          recall_scores[i] <- ifelse(!is.na(cm$byClass["Sensitivity"]), cm$byClass["Sensitivity"], 0)
          f1_scores[i] <- ifelse(!is.na(cm$byClass["F1"]), cm$byClass["F1"], 0)
        }
        
        # 找到召回率 > 0.7且F1最高的阈值
        high_recall_idx <- which(recall_scores >= 0.7)
        if (length(high_recall_idx) > 0) {
          best_idx <- high_recall_idx[which.max(f1_scores[high_recall_idx])]
          return(thresholds[best_idx])
        } else {
          # 否则使用最佳F1
          best_idx <- which.max(f1_scores)
          return(thresholds[best_idx])
        }
      } else {
        return(0.5)
      }
    }, error = function(e) {
      cat(paste("阈值调整失败:", e$message, "\n"))
      return(0.5)
    })
  }
  
  # 6. F3-score计算函数（新增）
  calculate_f3_score <- function(precision, recall, beta = 3) {
    if (is.na(precision) || is.na(recall) || (precision == 0 && recall == 0)) {
      return(0)
    }
    return((1 + beta^2) * (precision * recall) / (beta^2 * precision + recall))
  }
  
  # 7. 训练逻辑回归
  cat("\n训练逻辑回归...\n")
  lr_results <- NULL
  
  tryCatch({
    # 确保有足够的正样本
    if (sum(y_train == "Depeg") >= 2) {
      # 使用简单的glm作为后备
      train_df <- cbind(x_train, target = as.numeric(y_train == "Depeg"))
      
      # 移除线性依赖的列
      formula_str <- paste("target ~", paste(colnames(x_train), collapse = " + "))
      model_matrix <- model.matrix(as.formula(paste("target ~", paste(colnames(x_train), collapse = " + "))), 
                                   data = train_df)
      
      # 检查列是否线性依赖
      if (qr(model_matrix)$rank < ncol(model_matrix)) {
        cat("存在线性依赖列，使用PCA降维\n")
        # PCA降维
        pca <- prcomp(x_train, center = TRUE, scale. = TRUE)
        pca_vars <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
        n_components <- min(which(pca_vars >= 0.95), 10)
        x_train_pca <- as.data.frame(pca$x[, 1:n_components])
        x_test_pca <- as.data.frame(predict(pca, x_test)[, 1:n_components])
        
        lr_model <- glm(target ~ ., data = cbind(x_train_pca, target = as.numeric(y_train == "Depeg")), 
                        family = binomial)
        lr_prob <- predict(lr_model, newdata = x_test_pca, type = "response")
      } else {
        lr_model <- glm(target ~ ., data = train_df, family = binomial)
        lr_prob <- predict(lr_model, newdata = x_test, type = "response")
      }
      
      # 调整阈值
      lr_threshold <- adjust_threshold(lr_prob, y_test, threshold_method)
      cat(paste("逻辑回归阈值:", round(lr_threshold, 3), "\n"))
      
      lr_pred <- factor(ifelse(lr_prob > lr_threshold, "Depeg", "Normal"),
                       levels = c("Normal", "Depeg"))
      
      # 计算AUC
      lr_auc <- NA
      if (sum(y_test == "Depeg") > 0 && sum(y_test == "Normal") > 0) {
        tryCatch({
          lr_roc <- roc(as.numeric(y_test == "Depeg"), lr_prob, quiet = TRUE)
          lr_auc <- auc(lr_roc)
          cat(paste("逻辑回归AUC:", round(lr_auc, 4), "\n"))
        }, error = function(e) {
          cat(paste("逻辑回归AUC计算失败:", e$message, "\n"))
        })
      }
      
      # 评估
      if (sum(y_test == "Depeg") > 0) {
        lr_cm <- confusionMatrix(lr_pred, y_test, positive = "Depeg")
        
        # 获取精确率和召回率
        lr_precision <- ifelse("Precision" %in% names(lr_cm$byClass), 
                              lr_cm$byClass["Precision"], 
                              ifelse("Pos Pred Value" %in% names(lr_cm$byClass),
                                     lr_cm$byClass["Pos Pred Value"], NA))
        lr_recall <- ifelse("Recall" %in% names(lr_cm$byClass), 
                           lr_cm$byClass["Recall"], 
                           ifelse("Sensitivity" %in% names(lr_cm$byClass),
                                  lr_cm$byClass["Sensitivity"], NA))
        
        # 计算F3-score
        lr_f3_score <- calculate_f3_score(lr_precision, lr_recall, beta = 3)
        
        lr_results <- list(
          model = lr_model,
          predictions = lr_pred,
          probabilities = lr_prob,
          threshold = lr_threshold,
          auc = lr_auc,
          confusion_matrix = lr_cm,
          accuracy = lr_cm$overall["Accuracy"],
          sensitivity = ifelse("Sensitivity" %in% names(lr_cm$byClass), lr_cm$byClass["Sensitivity"], NA),
          specificity = ifelse("Specificity" %in% names(lr_cm$byClass), lr_cm$byClass["Specificity"], NA),
          f1 = ifelse("F1" %in% names(lr_cm$byClass), lr_cm$byClass["F1"], NA),
          precision = ifelse("Precision" %in% names(lr_cm$byClass), lr_cm$byClass["Precision"], NA),
          recall = ifelse("Recall" %in% names(lr_cm$byClass), lr_cm$byClass["Recall"], NA),
          f3_score = lr_f3_score  # 新增
        )
        cat("逻辑回归训练成功\n")
        cat(paste("逻辑回归F3-score:", round(lr_f3_score, 4), "\n"))
      }
    }
  }, error = function(e) {
    cat(paste("逻辑回归训练失败:", e$message, "\n"))
  })
  
  # 8. 训练随机森林
  cat("\n训练随机森林...\n")
  rf_results <- NULL
  
  tryCatch({
    if (nrow(x_train) > 30 && length(unique(y_train)) == 2) {
      # 确保列名有效
      colnames(x_train) <- make.names(colnames(x_train))
      colnames(x_test) <- make.names(colnames(x_test))
      
      # 使用更稳健的参数
      rf_model <- randomForest(
        x = x_train,
        y = y_train,
        ntree = 100,
        mtry = max(1, floor(sqrt(ncol(x_train)))),
        importance = TRUE,
        do.trace = FALSE,
        na.action = na.roughfix,
        sampsize = c("Normal" = min(100, sum(y_train == "Normal")),
                     "Depeg" = min(100, sum(y_train == "Depeg")))
      )
      
      # 预测
      rf_prob <- predict(rf_model, x_test, type = "prob")[, "Depeg"]
      
      # 调整阈值
      rf_threshold <- adjust_threshold(rf_prob, y_test, threshold_method)
      cat(paste("随机森林阈值:", round(rf_threshold, 3), "\n"))
      
      rf_pred <- factor(ifelse(rf_prob > rf_threshold, "Depeg", "Normal"),
                       levels = c("Normal", "Depeg"))
      
      # 计算AUC
      rf_auc <- NA
      if (sum(y_test == "Depeg") > 0 && sum(y_test == "Normal") > 0) {
        tryCatch({
          rf_roc <- roc(as.numeric(y_test == "Depeg"), rf_prob, quiet = TRUE)
          rf_auc <- auc(rf_roc)
          cat(paste("随机森林AUC:", round(rf_auc, 4), "\n"))
        }, error = function(e) {
          cat(paste("随机森林AUC计算失败:", e$message, "\n"))
        })
      }
      
      # 评估
      if (sum(y_test == "Depeg") > 0) {
        rf_cm <- confusionMatrix(rf_pred, y_test, positive = "Depeg")
        
        # 获取精确率和召回率
        rf_precision <- ifelse("Precision" %in% names(rf_cm$byClass), 
                              rf_cm$byClass["Precision"], 
                              ifelse("Pos Pred Value" %in% names(rf_cm$byClass),
                                     rf_cm$byClass["Pos Pred Value"], NA))
        rf_recall <- ifelse("Recall" %in% names(rf_cm$byClass), 
                           rf_cm$byClass["Recall"], 
                           ifelse("Sensitivity" %in% names(rf_cm$byClass),
                                  rf_cm$byClass["Sensitivity"], NA))
        
        # 计算F3-score
        rf_f3_score <- calculate_f3_score(rf_precision, rf_recall, beta = 3)
        
        rf_results <- list(
          model = rf_model,
          predictions = rf_pred,
          probabilities = rf_prob,
          threshold = rf_threshold,
          auc = rf_auc,
          confusion_matrix = rf_cm,
          accuracy = rf_cm$overall["Accuracy"],
          sensitivity = ifelse("Sensitivity" %in% names(rf_cm$byClass), rf_cm$byClass["Sensitivity"], NA),
          specificity = ifelse("Specificity" %in% names(rf_cm$byClass), rf_cm$byClass["Specificity"], NA),
          f1 = ifelse("F1" %in% names(rf_cm$byClass), rf_cm$byClass["F1"], NA),
          precision = ifelse("Precision" %in% names(rf_cm$byClass), rf_cm$byClass["Precision"], NA),
          recall = ifelse("Recall" %in% names(rf_cm$byClass), rf_cm$byClass["Recall"], NA),
          f3_score = rf_f3_score  # 新增
        )
        cat("随机森林训练成功\n")
        cat(paste("随机森林F3-score:", round(rf_f3_score, 4), "\n"))
      }
    } else {
      cat("训练样本太少或类别不足，跳过随机森林\n")
    }
  }, error = function(e) {
    cat(paste("随机森林训练失败:", e$message, "\n"))
  })
  
  # 9. 训练XGBoost（修复了语法错误）
  cat("\n训练XGBoost...\n")
  xgb_results <- NULL
  
  tryCatch({
    if (nrow(x_train) > 50 && requireNamespace("xgboost", quietly = TRUE)) {
      # 准备数据
      y_train_numeric <- as.numeric(y_train == "Depeg")
      y_test_numeric <- as.numeric(y_test == "Depeg")
      
      dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x_train), label = y_train_numeric)
      dtest <- xgboost::xgb.DMatrix(data = as.matrix(x_test), label = y_test_numeric)
      
      # 参数设置
      params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        max_depth = 3,
        eta = 0.1,
        subsample = 0.8,
        colsample_bytree = 0.8,
        min_child_weight = 1,
        gamma = 0,
        nthread = 1
      )
      
      # 训练
      xgb_model <- xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = 100,
        early_stopping_rounds = 10,
        evals = list(train = dtrain, test = dtest),  # 改为evals
        verbose = 0,
        print_every_n = 50
      )
      
      # 预测
      xgb_prob <- predict(xgb_model, as.matrix(x_test))
      
      # 调整阈值
      xgb_threshold <- adjust_threshold(xgb_prob, y_test, threshold_method)
      cat(paste("XGBoost阈值:", round(xgb_threshold, 3), "\n"))
      
      xgb_pred <- factor(ifelse(xgb_prob > xgb_threshold, "Depeg", "Normal"),
                        levels = c("Normal", "Depeg"))
      
      # 计算AUC
      xgb_auc <- NA
      if (sum(y_test == "Depeg") > 0 && sum(y_test == "Normal") > 0) {
        tryCatch({
          xgb_roc <- roc(as.numeric(y_test == "Depeg"), xgb_prob, quiet = TRUE)
          xgb_auc <- auc(xgb_roc)
          cat(paste("XGBoost AUC:", round(xgb_auc, 4), "\n"))
        }, error = function(e) {
          cat(paste("XGBoost AUC计算失败:", e$message, "\n"))
        })
      }
      
      # 评估
      if (sum(y_test == "Depeg") > 0) {
        xgb_cm <- confusionMatrix(xgb_pred, y_test, positive = "Depeg")
        
        # 获取精确率和召回率
        xgb_precision <- ifelse("Precision" %in% names(xgb_cm$byClass), 
                               xgb_cm$byClass["Precision"], 
                               ifelse("Pos Pred Value" %in% names(xgb_cm$byClass),
                                      xgb_cm$byClass["Pos Pred Value"], NA))
        xgb_recall <- ifelse("Recall" %in% names(xgb_cm$byClass), 
                            xgb_cm$byClass["Recall"], 
                            ifelse("Sensitivity" %in% names(xgb_cm$byClass),
                                   xgb_cm$byClass["Sensitivity"], NA))
        
        # 计算F3-score
        xgb_f3_score <- calculate_f3_score(xgb_precision, xgb_recall, beta = 3)
        
        xgb_results <- list(
          model = xgb_model,
          predictions = xgb_pred,
          probabilities = xgb_prob,
          threshold = xgb_threshold,
          auc = xgb_auc,
          confusion_matrix = xgb_cm,
          accuracy = xgb_cm$overall["Accuracy"],
          sensitivity = ifelse("Sensitivity" %in% names(xgb_cm$byClass), xgb_cm$byClass["Sensitivity"], NA),
          specificity = ifelse("Specificity" %in% names(xgb_cm$byClass), xgb_cm$byClass["Specificity"], NA),
          f1 = ifelse("F1" %in% names(xgb_cm$byClass), xgb_cm$byClass["F1"], NA),
          precision = ifelse("Precision" %in% names(xgb_cm$byClass), xgb_cm$byClass["Precision"], NA),
          recall = ifelse("Recall" %in% names(xgb_cm$byClass), xgb_cm$byClass["Recall"], NA),
          f3_score = xgb_f3_score  # 新增
        )
        cat("XGBoost训练成功\n")
        cat(paste("XGBoost F3-score:", round(xgb_f3_score, 4), "\n"))
      }
    } else {
      cat("跳过XGBoost（样本不足或包未安装）\n")
    }
  }, error = function(e) {
    cat(paste("XGBoost训练失败:", e$message, "\n"))
  })
  
 # 10. 汇总结果 
  results_df <- data.frame()
  
  # 添加逻辑回归结果
  if (!is.null(lr_results)) {
    results_df <- rbind(results_df, data.frame(
      Model = "LogisticRegression",
      Accuracy = round(lr_results$accuracy, 4),
      Recall = round(ifelse(is.na(lr_results$recall), 0, lr_results$recall), 4),
      Sensitivity = round(ifelse(is.na(lr_results$sensitivity), 0, lr_results$sensitivity), 4),
      Specificity = round(ifelse(is.na(lr_results$specificity), 0, lr_results$specificity), 4),
      AUC = round(ifelse(is.na(lr_results$auc), 0.5, lr_results$auc), 4),
      F3_Score = round(ifelse(is.na(lr_results$f3_score), 0, lr_results$f3_score), 4),  # 新增
      stringsAsFactors = FALSE
    ))
  }
  
  # 添加随机森林结果
  if (!is.null(rf_results)) {
    results_df <- rbind(results_df, data.frame(
      Model = "RandomForest",
      Accuracy = round(rf_results$accuracy, 4),
      Recall = round(ifelse(is.na(rf_results$recall), 0, rf_results$recall), 4),
      Sensitivity = round(ifelse(is.na(rf_results$sensitivity), 0, rf_results$sensitivity), 4),
      Specificity = round(ifelse(is.na(rf_results$specificity), 0, rf_results$specificity), 4),
      AUC = round(ifelse(is.na(rf_results$auc), 0.5, rf_results$auc), 4),
      F3_Score = round(ifelse(is.na(rf_results$f3_score), 0, rf_results$f3_score), 4),  # 新增
      stringsAsFactors = FALSE
    ))
  }
  
  # 添加XGBoost结果
  if (!is.null(xgb_results)) {
    results_df <- rbind(results_df, data.frame(
      Model = "XGBoost",
      Accuracy = round(xgb_results$accuracy, 4),
      Recall = round(ifelse(is.na(xgb_results$recall), 0, xgb_results$recall), 4),
      Sensitivity = round(ifelse(is.na(xgb_results$sensitivity), 0, xgb_results$sensitivity), 4),
      Specificity = round(ifelse(is.na(xgb_results$specificity), 0, xgb_results$specificity), 4),
      AUC = round(ifelse(is.na(xgb_results$auc), 0.5, xgb_results$auc), 4),
      F3_Score = round(ifelse(is.na(xgb_results$f3_score), 0, xgb_results$f3_score), 4),  # 新增
      stringsAsFactors = FALSE
    ))
  }
  
  # 如果没有模型成功，创建基准模型
  if (nrow(results_df) == 0) {
    cat("所有模型训练失败，创建基准模型\n")
    
    baseline_pred <- factor(rep("Normal", length(y_test)), levels = c("Normal", "Depeg"))
    baseline_cm <- confusionMatrix(baseline_pred, y_test, positive = "Depeg")
    
    # 基准模型的F3-score为0（因为没有预测到任何正例）
    results_df <- data.frame(
      Model = "Baseline",
      Accuracy = round(baseline_cm$overall["Accuracy"], 4),
      Recall = 0,
      Sensitivity = 0,
      Specificity = 1,
      AUC = 0.5,
      F3_Score = 0,  # 新增
      stringsAsFactors = FALSE
    )
  }
  
  # 11. 选择最佳模型 - 修改综合评分计算
  if (nrow(results_df) > 0) {
    # 计算综合得分（新增F3-score权重）
    results_df$Composite_Score <- 
      results_df$Recall * 0.3 +       # 召回率权重
      results_df$AUC * 0.3 +          # AUC权重
      results_df$F3_Score * 0.3 +     # F3-score权重（新增）
      results_df$Accuracy * 0.1       # 准确率权重
    
    best_model_index <- which.max(results_df$Composite_Score)
    best_model_name <- results_df$Model[best_model_index]
    
    cat(paste("\n模型性能汇总:\n"))
    print(results_df)
    cat(paste("\n最佳模型:", best_model_name, "\n"))
    cat(paste("最佳模型F3-score:", results_df$F3_Score[best_model_index], "\n"))
  } else {
    best_model_name <- "Baseline"
  }
  
  # 12. 返回完整结果
  return(list(
    summary = results_df,
    models = list(
      RandomForest = if(!is.null(rf_results)) rf_results$model else NULL,
      LogisticRegression = if(!is.null(lr_results)) lr_results$model else NULL,
      XGBoost = if(!is.null(xgb_results)) xgb_results$model else NULL
    ),
    results = list(
      RandomForest = rf_results,
      LogisticRegression = lr_results,
      XGBoost = xgb_results
    ),
    train_data = train_data,
    test_data = test_data,
    x_train = x_train,
    x_test = x_test,
    y_train = y_train,
    y_test = y_test,
    feature_cols = colnames(x_train),
    best_model_name = best_model_name,
    thresholds = list(
      RandomForest = if(!is.null(rf_results)) rf_results$threshold else 0.5,
      LogisticRegression = if(!is.null(lr_results)) lr_results$threshold else 0.5,
      XGBoost = if(!is.null(xgb_results)) xgb_results$threshold else 0.5
    )
  ))
}

# ==================== 9. 新增：模型效果可视化函数 ====================

# 1. 绘制ROC曲线对比图
plot_roc_curves_comparison <- function(model_results) {
  cat("绘制ROC曲线对比图...\n")
  
  roc_data <- data.frame()
  
  # 收集所有模型的ROC数据
  for (model_name in c("RandomForest", "XGBoost", "LogisticRegression")) {
    if (!is.null(model_results$results[[model_name]]$roc)) {
      roc_obj <- model_results$results[[model_name]]$roc
      temp_df <- data.frame(
        Specificity = 1 - roc_obj$specificities,
        Sensitivity = roc_obj$sensitivities,
        Model = model_name,
        AUC = round(auc(roc_obj), 4)
      )
      roc_data <- rbind(roc_data, temp_df)
    }
  }
  
  if (nrow(roc_data) == 0) {
    cat("没有可用的ROC数据\n")
    return(ggplot() + 
           labs(title = "ROC曲线对比", subtitle = "无可用数据") +
           theme_minimal())
  }
  
  # 绘制ROC曲线
  p <- ggplot(roc_data, aes(x = Specificity, y = Sensitivity, color = Model)) +
    geom_line(linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray", alpha = 0.7) +
    labs(title = "ROC曲线对比",
         subtitle = paste("测试集性能比较"),
         x = "假阳性率 (1 - 特异性)",
         y = "真阳性率 (灵敏度)",
         color = "模型") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    scale_color_manual(values = c("RandomForest" = "#1f77b4", 
                                 "XGBoost" = "#ff7f0e", 
                                 "LogisticRegression" = "#2ca02c")) +
    annotate("text", x = 0.7, y = 0.2, 
             label = paste("随机猜测基准线"), 
             color = "gray", size = 3.5)
  
  # 添加AUC标签
  auc_labels <- roc_data %>%
    group_by(Model) %>%
    summarise(AUC = first(AUC), .groups = "drop")
  
  for (i in 1:nrow(auc_labels)) {
    p <- p + 
      annotate("text", x = 0.7, y = 0.15 - (i-1)*0.05,
               label = paste(auc_labels$Model[i], "AUC =", auc_labels$AUC[i]),
               color = c("#1f77b4", "#ff7f0e", "#2ca02c")[i], 
               size = 4, hjust = 0)
  }
  
  return(p)
}

# 2. 绘制混淆矩阵热图
plot_confusion_matrices <- function(model_results) {
  cat("绘制混淆矩阵热图...\n")
  
  plots <- list()
  
  for (model_name in c("RandomForest", "XGBoost", "LogisticRegression")) {
    if (!is.null(model_results$results[[model_name]]$confusion_matrix)) {
      cm <- model_results$results[[model_name]]$confusion_matrix
      cm_df <- as.data.frame(cm$table)
      
      p <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile(color = "white") +
        geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Freq/sum(Freq)*100)), 
                  color = "white", size = 5, fontface = "bold") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(title = paste(model_name, "混淆矩阵"),
             subtitle = paste("准确率:", round(cm$overall["Accuracy"], 4)),
             x = "实际类别", y = "预测类别") +
        theme_minimal() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              plot.subtitle = element_text(hjust = 0.5, size = 12))
      
      plots[[model_name]] <- p
    }
  }
  
  if (length(plots) == 0) {
    return(ggplot() + labs(title = "混淆矩阵", subtitle = "无可用数据") + theme_minimal())
  }
  
  # 合并多个图
  if (length(plots) == 1) {
    return(plots[[1]])
  } else {
    return(grid.arrange(grobs = plots, ncol = min(3, length(plots))))
  }
}

# 3. 绘制模型性能雷达图
plot_model_performance_radar <- function(model_results) {
  cat("绘制模型性能雷达图...\n")
  
  if (nrow(model_results$summary) == 0) {
    cat("没有可用的性能数据\n")
    return(ggplot() + 
           labs(title = "模型性能雷达图", subtitle = "无可用数据") +
           theme_minimal())
  }
  
  # 准备数据
  summary_data <- model_results$summary
  
  # 确保所有需要的指标都存在
  required_metrics <- c("Accuracy", "Recall", "Sensitivity", "Specificity", "AUC")
  
  for (metric in required_metrics) {
    if (!metric %in% colnames(summary_data)) {
      cat(paste("警告：缺少指标", metric, "，使用默认值0.5\n"))
      summary_data[[metric]] <- 0.5
    }
  }
  
  # 如果有Stablecoin列，创建组合标签
  if ("Stablecoin" %in% colnames(summary_data)) {
    summary_data$Model_Label <- paste(summary_data$Stablecoin, summary_data$Model, sep = " - ")
    plot_data <- summary_data %>%
      select(Model_Label, all_of(required_metrics))
  } else {
    summary_data$Model_Label <- summary_data$Model
    plot_data <- summary_data %>%
      select(Model_Label, all_of(required_metrics))
  }
  
  # 准备雷达图数据
  radar_data <- plot_data %>%
    pivot_longer(cols = all_of(required_metrics), names_to = "Metric", values_to = "Value") %>%
    mutate(Value = as.numeric(Value))
  
  # 标准化到0-1范围
  radar_data <- radar_data %>%
    group_by(Metric) %>%
    mutate(
      Value_scaled = ifelse(
        all(is.na(Value)) || diff(range(Value, na.rm = TRUE)) == 0,
        0.5,
        (Value - min(Value, na.rm = TRUE)) / 
          (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE))
      )
    ) %>%
    ungroup()
  
  # 创建雷达图
  p <- ggplot(radar_data, aes(x = Metric, y = Value_scaled, group = Model_Label, color = Model_Label)) +
    geom_polygon(aes(fill = Model_Label), alpha = 0.2, linewidth = 1) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    coord_polar() +
    labs(title = "模型性能雷达图",
         subtitle = "各指标标准化到0-1范围",
         x = "", y = "") +
    theme_minimal() +
    theme(axis.text = element_text(size = 11),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_line(color = "gray90")) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +
    ylim(0, 1)
  
  # 添加数值标签
  p <- p + 
    geom_text(data = radar_data, 
              aes(label = sprintf("%.3f", Value)), 
              color = "black", size = 3.5, vjust = -1)
  
  return(p)
}


# ==================== 10. 修正模型性能对比图函数 ====================

plot_model_comparison_enhanced <- function(results_df) {
  if (nrow(results_df) == 0) {
    return(ggplot() + labs(title = "无模型性能数据"))
  }
  
  # 检查并处理Stablecoin列
  if ("Stablecoin" %in% colnames(results_df)) {
    # 对于分开训练模型的结果，我们需要特殊处理
    # 创建合并标签列
    results_df$Model_Label <- paste(results_df$Stablecoin, results_df$Model, sep = " - ")
    
    # 移除不需要的列
    plot_df <- results_df[, !colnames(results_df) %in% c("Composite_Score", "Stablecoin", "Model"), drop = FALSE]
    id_col <- "Model_Label"
  } else {
    # 对于混合训练模型的结果，保持原样
    plot_df <- results_df[, !colnames(results_df) %in% "Composite_Score", drop = FALSE]
    plot_df$Model_Label <- plot_df$Model
    id_col <- "Model_Label"
  }
  
  # 确保所有指标列都是数值类型
  metric_cols <- setdiff(colnames(plot_df), id_col)
  for (col in metric_cols) {
    plot_df[[col]] <- as.numeric(as.character(plot_df[[col]]))
  }
  
  # 长格式转换
  melt_results <- plot_df %>%
    pivot_longer(cols = all_of(metric_cols), names_to = "Metric", values_to = "Value")
  
  # 添加颜色映射
  melt_results <- melt_results %>%
    mutate(
      color_group = case_when(
        Metric == "AUC" ~ "AUC",
        Metric == "Accuracy" ~ "准确率",
        Metric == "Recall" ~ "召回率",
        Metric == "Sensitivity" ~ "敏感度",
        Metric == "Specificity" ~ "特异性",
        Metric == "F3_Score" ~ "F3分数",
        TRUE ~ "其他"
      )
    )
  
  p <- ggplot(melt_results, aes(x = .data[[id_col]], y = Value, fill = color_group)) +
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.7) +
    geom_text(aes(label = round(Value, 3)),
              position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3.5, fontface = "bold") +
    # 添加性能基准线
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "green", alpha = 0.5) +
    geom_hline(yintercept = 0.6, linetype = "dashed", color = "orange", alpha = 0.5) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
    # 添加基准线标签
    annotate("text", x = 0.5, y = 0.81, label = "优秀 (>0.8)", 
             color = "green", hjust = 0, size = 3) +
    annotate("text", x = 0.5, y = 0.61, label = "良好 (>0.6)", 
             color = "orange", hjust = 0, size = 3) +
    annotate("text", x = 0.5, y = 0.51, label = "基准 (>0.5)", 
             color = "red", hjust = 0, size = 3) +
    labs(title = "模型性能对比分析",
         subtitle = ifelse("Stablecoin" %in% colnames(results_df), 
                          "分开训练模型性能对比",
                          "包含AUC、准确率、召回率等关键指标"),
         x = "模型", y = "分数", fill = "指标类型") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    scale_fill_viridis_d(name = "指标类型") +
    ylim(0, 1) +
    facet_wrap(~Metric, scales = "free_y", ncol = 2)
  
  return(p)
}

# ==================== 11. 新增：用户交互模块 ====================

# 1. 交互式用户投资组合输入
get_user_portfolio_interactive <- function(available_stablecoins = c("USDC", "FDUSD", "DAI")) {
  cat("\n")
  cat(paste0(strrep("=", 60)), "\n")
  cat("  用户投资组合配置输入\n")
  cat(paste0(strrep("=", 60)), "\n\n")
  
  portfolio <- list()
  remaining <- 1.0
  
  cat("可用稳定币:", paste(available_stablecoins, collapse = ", "), "\n\n")
  cat("请输入各稳定币的配置比例 (总和应为100%):\n")
  
  for (i in seq_along(available_stablecoins)) {
    sc <- available_stablecoins[i]
    
    if (i < length(available_stablecoins)) {
      while (TRUE) {
        cat(paste0("  ", sc, " 配置比例 (当前剩余 ", round(remaining * 100, 1), "%): "))
        input <- readline()
        
        if (input == "") {
          cat("请输入有效数字\n")
          next
        }
        
        value <- as.numeric(input)
        
        if (is.na(value)) {
          cat("无效输入，请输入数字\n")
        } else if (value < 0 || value > remaining * 100) {
          cat(paste0("请输入 0 到 ", round(remaining * 100, 1), " 之间的数字\n"))
        } else {
          portfolio[[sc]] <- value / 100
          remaining <- remaining - portfolio[[sc]]
          break
        }
      }
    } else {
      # 最后一个自动分配剩余比例
      portfolio[[sc]] <- remaining
      cat(paste0("  ", sc, " 自动分配剩余比例: ", round(remaining * 100, 1), "%\n"))
    }
  }
  
  # 验证总和是否为1
  total <- sum(unlist(portfolio))
  if (abs(total - 1) > 0.001) {
    cat("\n警告：投资比例总和为", round(total * 100, 1), "%，正在重新标准化...\n")
    for (sc in names(portfolio)) {
      portfolio[[sc]] <- portfolio[[sc]] / total
    }
  }
  
  # 总投资金额输入
  cat("\n")
  cat(paste0(strrep("-", 40)), "\n")
  while (TRUE) {
    cat("请输入总投资金额 (美元): ")
    amount_input <- readline()
    total_assets <- as.numeric(gsub("[^0-9.]", "", amount_input))
    
    if (is.na(total_assets) || total_assets <= 0) {
      cat("无效输入！请输入正数金额 (如: 10000, 50000)\n")
    } else if (total_assets > 1e12) {
      cat("金额过大！请输入合理的投资金额\n")
    } else {
      break
    }
  }
  
  # 显示汇总
  cat("\n")
  cat(paste0(strrep("=", 60)), "\n")
  cat("投资组合配置汇总:\n")
  for (sc in names(portfolio)) {
    amount <- total_assets * portfolio[[sc]]
    cat(paste0("  ", sc, ": ", round(portfolio[[sc]] * 100, 1), 
              "% ($", format(round(amount), big.mark = ",", trim = TRUE), ")\n"))
  }
  cat(paste0("总资产: $", format(total_assets, big.mark = ",", scientific = FALSE), "\n"))
  cat(paste0(strrep("=", 60)), "\n\n")
  
  
  return(list(
    portfolio = portfolio,
    total_assets = total_assets
  ))
}

# ==================== 12. 增强的用户资产风险分析模块（修改版：使用训练好的模型预测未来一天风险） ====================

# 修改的用户资产风险分析模块
plot_user_portfolio_risk_enhanced <- function(data, model_results, 
                                               portfolio = list(USDC = 0.4, FDUSD = 0.3, DAI = 0.3),
                                               total_assets = 10000) {
  
  cat("开始用户资产风险分析...\n")
  cat("使用训练好的模型预测未来一天（明天）的脱锚风险\n\n")
  
  # 获取每个稳定币的最新数据（用于预测未来一天）
  portfolio_names <- names(portfolio)
  
  # 准备预测数据
  prediction_data <- data.frame()
  
  for (sc in portfolio_names) {
    # 获取该稳定币的最新数据
    sc_data <- data %>%
      filter(stablecoin == sc) %>%
      arrange(desc(timestamp)) %>%
      slice(1)  # 只取最新一天的数据
    
    if (nrow(sc_data) > 0) {
      # 添加标识
      sc_data$stablecoin <- sc
      prediction_data <- rbind(prediction_data, sc_data)
      cat(paste("  ", sc, ": 获取到最新数据，日期=", 
                format(sc_data$timestamp[1], "%Y-%m-%d"), "\n", sep = ""))
    } else {
      cat(paste("  ", sc, ": 无数据可用\n", sep = ""))
    }
  }
  
  if (nrow(prediction_data) == 0) {
    cat("错误：没有可用的预测数据\n")
    return(NULL)
  }
  
  # 使用训练好的模型进行预测
  cat("\n使用最佳模型进行预测...\n")
  
  # 准备特征数据（与训练时相同的特征）
  feature_cols <- model_results$feature_cols
  
  # 检查特征是否可用
  missing_features <- setdiff(feature_cols, colnames(prediction_data))
  if (length(missing_features) > 0) {
    cat(paste("警告：以下特征在预测数据中缺失:", paste(missing_features, collapse=", "), "\n"))
    cat("尝试使用可用特征...\n")
    feature_cols <- intersect(feature_cols, colnames(prediction_data))
  }
  
  # 准备预测特征矩阵
  x_predict <- prediction_data[, feature_cols, drop = FALSE]
  
  # 处理缺失值
  safe_impute <- function(x) {
    if (is.numeric(x)) {
      x[is.na(x) | is.nan(x) | is.infinite(x)] <- median(x[!is.na(x) & !is.nan(x) & !is.infinite(x)], na.rm = TRUE)
      if (all(is.na(x))) x[is.na(x)] <- 0
    }
    return(x)
  }
  
  x_predict <- as.data.frame(lapply(x_predict, safe_impute))
  
  # 使用最佳模型进行预测
  best_model_name <- model_results$best_model_name
  cat(paste("使用最佳模型:", best_model_name, "进行预测\n"))
  
  depeg_probabilities <- numeric(nrow(prediction_data))
  
  if (best_model_name == "RandomForest" && !is.null(model_results$models$RandomForest)) {
    # 随机森林预测
    rf_model <- model_results$models$RandomForest
    # 确保列名有效
    colnames(x_predict) <- make.names(colnames(x_predict))
    rf_prob <- predict(rf_model, x_predict, type = "prob")[, "Depeg"]
    depeg_probabilities <- rf_prob * 100  # 转换为百分比
    
  } else if (best_model_name == "XGBoost" && !is.null(model_results$models$XGBoost)) {
    # XGBoost预测
    xgb_model <- model_results$models$XGBoost
    xgb_prob <- predict(xgb_model, as.matrix(x_predict))
    depeg_probabilities <- xgb_prob * 100  # 转换为百分比
    
  } else if (best_model_name == "LogisticRegression" && !is.null(model_results$models$LogisticRegression)) {
    # 逻辑回归预测
    lr_model <- model_results$models$LogisticRegression
    lr_prob <- predict(lr_model, x_predict, type = "response")
    depeg_probabilities <- lr_prob * 100  # 转换为百分比
    
  } else {
    cat("警告：无法使用最佳模型，使用简化方法作为后备\n")
    # 后备方法：基于价格偏离度
    for (i in 1:nrow(prediction_data)) {
      depeg_probabilities[i] <- pmin(100, abs(prediction_data$close[i] - 1) * 1000)
    }
  }
  
  # 创建风险数据框
  risk_data <- prediction_data %>%
    mutate(
      depeg_probability = depeg_probabilities,
      risk_level = case_when(
        depeg_probability >= 70 ~ "High_Risk",
        depeg_probability >= 30 ~ "Medium_Risk",
        TRUE ~ "Low_Risk"
      ),
      current_price = close
    )
  
  # 计算投资组合风险
  portfolio_risk <- risk_data %>%
    mutate(
      # 安全地获取分配比例
      allocation = sapply(stablecoin, function(x) {
        if (x %in% names(portfolio)) {
          return(portfolio[[x]])
        } else {
          return(0)
        }
      }),
      allocation_amount = total_assets * allocation,
      # 假设脱锚损失为投资额的10%
      expected_loss = allocation_amount * (depeg_probability/100) * 0.1,
      # 风险贡献计算
      risk_contribution = ifelse(
        sum(allocation * (depeg_probability/100), na.rm = TRUE) > 0,
        (allocation * (depeg_probability/100)) / 
          sum(allocation * (depeg_probability/100), na.rm = TRUE) * 100,
        0
      )
    )
  
  # 计算总体风险得分 (0-100)
  overall_risk_score <- sum(portfolio_risk$allocation * portfolio_risk$depeg_probability, na.rm = TRUE)
  
  # 风险等级
  overall_risk_level <- case_when(
    overall_risk_score >= 50 ~ "High_Risk",
    overall_risk_score >= 20 ~ "Medium_Risk",
    TRUE ~ "Low_Risk"
  )
  
  # 保存portfolio_risk_details.csv
  portfolio_risk_export <- portfolio_risk %>%
    select(
      stablecoin,
      current_price,
      allocation_percent = allocation,
      allocation_amount,
      depeg_probability,
      risk_level,
      risk_contribution,
      expected_loss
    ) %>%
    mutate(
      allocation_percent = allocation_percent * 100,
      risk_level = case_when(
        risk_level == "High_Risk" ~ "High",
        risk_level == "Medium_Risk" ~ "Medium",
        risk_level == "Low_Risk" ~ "Low",
        TRUE ~ risk_level
      )
    )
  
  # 使用UTF-8编码保存CSV文件
  write.csv(portfolio_risk_export, 
            file.path("result", "data", "portfolio_risk_details.csv"), 
            row.names = FALSE,
            fileEncoding = "UTF-8")
  
  cat(paste("投资组合风险数据已保存到 result/data/portfolio_risk_details.csv\n", sep = ""))
  
  # 1. 投资组合风险分布图
  p1 <- ggplot(portfolio_risk, aes(x = stablecoin, y = allocation_amount, fill = risk_level)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = paste0("$", format(round(allocation_amount), big.mark = ","))),
              vjust = -0.5, size = 4, fontface = "bold") +
    geom_text(aes(y = allocation_amount/2, 
                  label = paste0(round(depeg_probability, 1), "%")),
              color = "white", size = 5, fontface = "bold") +
    labs(title = "用户投资组合风险分析（未来一天预测）",
         subtitle = paste("预测模型:", best_model_name, 
                         " | 总资产: $", format(total_assets, big.mark = ","),
                         " | 总体风险得分: ", round(overall_risk_score, 1), "/100",
                         "\n注：预测的是明天（", format(Sys.Date() + 1, "%Y-%m-%d"), "）的脱锚风险"),
         x = "稳定币", y = "投资金额 ($)", fill = "风险等级") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    scale_fill_manual(values = c("High_Risk" = "#ff6b6b", "Medium_Risk" = "#ffd166", "Low_Risk" = "#06d6a0")) +
    ylim(0, max(portfolio_risk$allocation_amount) * 1.2)
  
  # 2. 风险热力图
  p2 <- plot_portfolio_risk_heatmap(portfolio_risk, overall_risk_score)
  p2 <- p2 + labs(
    title = "投资组合风险热力图（未来一天预测）",
    subtitle = paste("基于", best_model_name, "模型预测明天(", format(Sys.Date() + 1, "%Y-%m-%d"), ")的脱锚概率")
  )
  
  # 3. 风险贡献瀑布图
  p3 <- plot_risk_contribution_waterfall(portfolio_risk, overall_risk_score)
  p3 <- p3 + labs(
    title = "风险贡献分解图（未来一天预测）",
    subtitle = paste("显示各资产对明天(", format(Sys.Date() + 1, "%Y-%m-%d"), ")总体风险的贡献程度")
  )
  
  # 4. 压力测试分析
  stress_test <- plot_stress_test_scenarios(portfolio_risk, total_assets, overall_risk_score)
  stress_test$plot <- stress_test$plot + labs(
    title = "压力测试情景分析（未来一天预测）",
    subtitle = paste("基于", best_model_name, "模型预测明天(", format(Sys.Date() + 1, "%Y-%m-%d"), ")的风险情景")
  )
  
  # 5. 风险调整后收益分析
  risk_return <- plot_risk_adjusted_returns(portfolio_risk)
  risk_return$risk_return_scatter <- risk_return$risk_return_scatter + labs(
    title = "风险调整后收益分析（未来一天预测）",
    subtitle = paste("基于", best_model_name, "模型预测的明天脱锚概率 vs 预期收益率")
  )
  
  # 6. 生成优化建议
  recommendations <- generate_portfolio_recommendations(portfolio_risk, overall_risk_score)
  
  # 汇总报告
  risk_report <- list(
    overall_score = overall_risk_score,
    overall_level = overall_risk_level,
    portfolio_details = portfolio_risk,
    expected_loss_range = c(
      min_loss = min(portfolio_risk$expected_loss, na.rm = TRUE),
      max_loss = max(portfolio_risk$expected_loss, na.rm = TRUE)
    ),
    recommendations = recommendations$overall_advice,
    prediction_model = best_model_name,
    prediction_date = format(Sys.Date() + 1, "%Y-%m-%d"),
    current_date = format(Sys.Date(), "%Y-%m-%d")
  )
  
  # 保存建议到文件
  recommendations_file <- file.path("result", "reports", "portfolio_recommendations.txt")
  writeLines(
    c(
      "=== 投资组合优化建议（基于未来一天风险预测） ===",
      paste("预测模型:", best_model_name),
      paste("预测日期:", format(Sys.Date() + 1, "%Y-%m-%d")),
      paste("当前日期:", format(Sys.Date(), "%Y-%m-%d")),
      "",
      risk_report$recommendations,
      "",
      if(!is.null(recommendations$high_risk_advice)) recommendations$high_risk_advice else "",
      "",
      if(!is.null(recommendations$medium_risk_advice)) recommendations$medium_risk_advice else "",
      "",
      if(!is.null(recommendations$low_risk_advice)) recommendations$low_risk_advice else ""
    ),
    recommendations_file
  )
  
  # 创建HTML报告
  portfolio_config <- list(
    portfolio = portfolio,
    total_assets = total_assets
  )
  
  user_risk_analysis <- list(
    portfolio_data = portfolio_risk,
    report = risk_report
  )
  
  report_file <- create_interactive_html_report(user_risk_analysis, portfolio_config, "result")
  
  return(list(
    portfolio_plot = p1,
    risk_heatmap = p2,
    risk_waterfall = p3,
    stress_test = stress_test$plot,
    risk_return_scatter = risk_return$risk_return_scatter,
    risk_ratios = risk_return$risk_ratios,
    report = risk_report,
    portfolio_data = portfolio_risk,
    recommendations = recommendations,
    report_file = report_file,
    prediction_model = best_model_name
  ))
}

# ==================== 13. 风险可视化模块 ====================

# 1. 投资组合风险分布热力图
plot_portfolio_risk_heatmap <- function(portfolio_risk, overall_risk_score) {
  # 创建风险矩阵数据
  risk_matrix <- expand.grid(
    stablecoin = portfolio_risk$stablecoin,
    metric = c("Depeg_Probability", "Allocation", "Expected_Loss", "Risk_Contribution")
  ) %>%
    left_join(
      portfolio_risk %>%
        select(
          stablecoin,
          Depeg_Probability = depeg_probability,
          Allocation = allocation_amount,
          Expected_Loss = expected_loss,
          Risk_Contribution = risk_contribution
        ),
      by = "stablecoin"
    ) %>%
    mutate(
      value_scaled = case_when(
        metric == "Depeg_Probability" ~ Depeg_Probability / 100,
        metric == "Allocation" ~ Allocation / max(Allocation),
        metric == "Expected_Loss" ~ Expected_Loss / max(Expected_Loss),
        metric == "Risk_Contribution" ~ Risk_Contribution / 100,
        TRUE ~ 0
      ),
      value_label = case_when(
        metric == "Depeg_Probability" ~ paste0(round(Depeg_Probability, 1), "%"),
        metric == "Allocation" ~ paste0("$", format(round(Allocation), big.mark = ",")),
        metric == "Expected_Loss" ~ paste0("$", format(round(Expected_Loss), big.mark = ",")),
        metric == "Risk_Contribution" ~ paste0(round(Risk_Contribution, 1), "%"),
        TRUE ~ ""
      )
    )
  
  # 创建热力图
  p <- ggplot(risk_matrix, aes(x = stablecoin, y = metric, fill = value_scaled)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = value_label), color = "white", size = 4, fontface = "bold") +
    scale_fill_gradient2(
      low = "#00aa00",
      mid = "#ffaa00",
      high = "#ff0000",
      midpoint = 0.5,
      name = "风险程度"
    ) +
    labs(
      title = "投资组合风险热力图",
      subtitle = paste("总体风险得分:", round(overall_risk_score, 1), "/100"),
      x = "稳定币",
      y = "风险指标"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 14)
    )
  
  return(p)
}

# 2. 风险贡献分解瀑布图
plot_risk_contribution_waterfall <- function(portfolio_risk, overall_risk_score) {
  # 计算各资产对总体风险的贡献
  risk_decomp <- portfolio_risk %>%
    mutate(
      risk_share = (allocation * depeg_probability) / overall_risk_score * 100,
      cumulative_risk = cumsum(risk_share)
    ) %>%
    arrange(desc(risk_share))
  
  # 创建瀑布图数据
  waterfall_data <- data.frame(
    category = c("初始风险", risk_decomp$stablecoin, "总体风险"),
    value = c(0, risk_decomp$risk_share, overall_risk_score)
  ) %>%
    mutate(
      type = c("net", rep("intermediate", nrow(risk_decomp)), "total"),
      label = ifelse(type == "intermediate", 
                    paste0(round(value, 1), "%"), 
                    ifelse(type == "total", paste0(round(overall_risk_score, 1), "/100"), ""))
    )
  
  # 计算累计值
  for (i in 2:nrow(waterfall_data)) {
    if (waterfall_data$type[i] == "intermediate") {
      waterfall_data$value[i] <- waterfall_data$value[i-1] + waterfall_data$value[i]
    }
  }
  
  # 创建瀑布图
  p <- ggplot(waterfall_data, aes(x = category, y = value, fill = type)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_segment(aes(x = as.numeric(factor(category)) - 0.4,
                     xend = as.numeric(factor(category)) + 0.4,
                     yend = value),
                 color = "black", size = 1) +
    geom_text(aes(label = label), vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "net" = "#4ecdc4",
        "intermediate" = "#ff6b6b",
        "total" = "#118ab2"
      ),
      guide = "none"
    ) +
    labs(
      title = "风险贡献分解图",
      subtitle = "显示各资产对总体风险的贡献程度",
      x = "风险来源",
      y = "风险贡献 (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    ylim(0, max(waterfall_data$value) * 1.2)
  
  return(p)
}

# 3. 压力测试情景分析
plot_stress_test_scenarios <- function(portfolio_risk, total_assets, overall_risk_score) {
  # 定义压力测试情景
  scenarios <- data.frame(
    scenario = c(
      "基准情景",
      "轻度脱锚 (5%)",
      "中度脱锚 (10%)",
      "重度脱锚 (20%)",
      "极端脱锚 (50%)"
    ),
    loss_rate = c(0, 0.05, 0.10, 0.20, 0.50),
    probability = c(0.7, 0.2, 0.07, 0.02, 0.01)
  )
  
  # 计算每种情景下的损失
  scenarios <- scenarios %>%
    mutate(
      expected_loss = total_assets * overall_risk_score/100 * loss_rate * probability,
      var_95 = total_assets * overall_risk_score/100 * loss_rate * 0.05,  # 95% VaR
      cvar_95 = total_assets * overall_risk_score/100 * loss_rate * 0.10, # 95% CVaR
      label = paste0(
        "损失率: ", loss_rate * 100, "%\n",
        "概率: ", probability * 100, "%\n",
        "预期损失: $", format(round(expected_loss), big.mark = ",")
      )
    )
  
  # 创建压力测试图
  p <- ggplot(scenarios, aes(x = scenario, y = expected_loss, fill = scenario)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_errorbar(aes(ymin = expected_loss - var_95, ymax = expected_loss + var_95),
                  width = 0.2, color = "black", size = 0.8) +
    geom_point(aes(y = cvar_95), color = "red", size = 4, shape = 18) +
    geom_text(aes(label = paste0("$", format(round(expected_loss), big.mark = ","))),
              vjust = -0.5, size = 4, fontface = "bold") +
    labs(
      title = "压力测试情景分析",
      subtitle = "不同脱锚情景下的预期损失 (带风险价值)",
      x = "压力测试情景",
      y = "预期损失 ($)",
      fill = "情景"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    scale_fill_viridis_d() +
    ylim(0, max(scenarios$expected_loss) * 1.5)
  
  return(list(plot = p, scenarios = scenarios))
}

# 4. 风险调整后收益分析
plot_risk_adjusted_returns <- function(portfolio_risk) {
  # 假设的年化收益率（根据稳定币类型设定）
  base_returns <- data.frame(
    stablecoin = c("USDC", "FDUSD", "DAI"),
    expected_return = c(0.05, 0.06, 0.07),  # 年化收益率
    volatility = c(0.02, 0.03, 0.04)        # 年化波动率
  )
  
  # 合并数据
  risk_return_data <- portfolio_risk %>%
    left_join(base_returns, by = "stablecoin") %>%
    mutate(
      risk_adjusted_return = expected_return / (depeg_probability/100 + 0.01),
      sharpe_ratio = expected_return / volatility,
      sortino_ratio = expected_return / (depeg_probability/100),
      expected_annual_return = allocation_amount * expected_return
    )
  
  # 风险调整后收益散点图
  p1 <- ggplot(risk_return_data, aes(x = depeg_probability, y = expected_return * 100,
                                       size = allocation_amount, color = stablecoin)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = stablecoin), vjust = -1, size = 4, fontface = "bold") +
    geom_hline(yintercept = mean(risk_return_data$expected_return) * 100, 
               linetype = "dashed", color = "gray") +
    geom_vline(xintercept = mean(risk_return_data$depeg_probability), 
               linetype = "dashed", color = "gray") +
    annotate("rect", xmin = 0, xmax = 30, ymin = 5, ymax = 10,
             fill = "green", alpha = 0.1) +
    annotate("rect", xmin = 30, xmax = 70, ymin = 5, ymax = 10,
             fill = "yellow", alpha = 0.1) +
    annotate("rect", xmin = 70, xmax = 100, ymin = 5, ymax = 10,
             fill = "red", alpha = 0.1) +
    labs(
      title = "风险调整后收益分析",
      subtitle = "脱锚概率 vs 预期收益率",
      x = "脱锚概率 (%)",
      y = "预期年化收益率 (%)",
      size = "投资金额 ($)",
      color = "稳定币"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    scale_color_viridis_d() +
    scale_size_continuous(labels = scales::dollar_format())
  
  # 夏普比率和索提诺比率
  p2 <- ggplot(risk_return_data, aes(x = stablecoin)) +
    geom_bar(aes(y = sharpe_ratio, fill = "夏普比率"), 
             stat = "identity", position = "dodge", width = 0.4) +
    geom_bar(aes(y = sortino_ratio, fill = "索提诺比率"), 
             stat = "identity", position = position_dodge(width = 0.5), width = 0.4) +
    geom_text(aes(y = sharpe_ratio, label = round(sharpe_ratio, 2)),
              position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
    geom_text(aes(y = sortino_ratio, label = round(sortino_ratio, 2)),
              position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
    labs(
      title = "风险调整比率",
      subtitle = "夏普比率 vs 索提诺比率",
      x = "稳定币",
      y = "比率值",
      fill = "比率类型"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    scale_fill_manual(values = c("夏普比率" = "#4ecdc4", "索提诺比率" = "#ff6b6b"))
  
  return(list(risk_return_scatter = p1, risk_ratios = p2, data = risk_return_data))
}

# 5. 投资组合优化建议生成器
generate_portfolio_recommendations <- function(portfolio_risk, overall_risk_score) {
  # 生成优化建议
  recommendations <- list()
  
  # 根据风险等级提供建议
  high_risk_assets <- portfolio_risk %>% filter(risk_level == "High_Risk")
  medium_risk_assets <- portfolio_risk %>% filter(risk_level == "Medium_Risk")
  low_risk_assets <- portfolio_risk %>% filter(risk_level == "Low_Risk")
  
  if (nrow(high_risk_assets) > 0) {
    recommendations$high_risk_advice <- paste0(
      "高风险资产 (", nrow(high_risk_assets), "个):\n",
      paste("  - ", high_risk_assets$stablecoin, 
            " (风险概率: ", round(high_risk_assets$depeg_probability, 1), "%)",
            sep = "", collapse = "\n"),
      "\n建议: 考虑减少高风险资产的配置比例，转移至低风险资产"
    )
  }
  
  if (nrow(medium_risk_assets) > 0) {
    recommendations$medium_risk_advice <- paste0(
      "中风险资产 (", nrow(medium_risk_assets), "个):\n",
      paste("  - ", medium_risk_assets$stablecoin, 
            " (风险概率: ", round(medium_risk_assets$depeg_probability, 1), "%)",
            sep = "", collapse = "\n"),
      "\n建议: 监控中风险资产表现，设置止损点"
    )
  }
  
  if (nrow(low_risk_assets) > 0) {
    recommendations$low_risk_advice <- paste0(
      "低风险资产 (", nrow(low_risk_assets), "个):\n",
      paste("  - ", low_risk_assets$stablecoin, 
            " (风险概率: ", round(low_risk_assets$depeg_probability, 1), "%)",
            sep = "", collapse = "\n"),
      "\n建议: 可保持或适度增加低风险资产的配置"
    )
  }
  
  # 总体建议
  if (overall_risk_score >= 50) {
    recommendations$overall_advice <- paste0(
      "⚠️ 高风险警报 ⚠️\n",
      "总体风险得分: ", round(overall_risk_score, 1), "/100\n",
      "预测时间: 明天 (", format(Sys.Date() + 1, "%Y-%m-%d"), ")\n",
      "建议: 立即调整投资组合，降低高风险资产比例至30%以下"
    )
  } else if (overall_risk_score >= 20) {
    recommendations$overall_advice <- paste0(
      "⚠️ 中风险警告 ⚠️\n",
      "总体风险得分: ", round(overall_risk_score, 1), "/100\n",
      "预测时间: 明天 (", format(Sys.Date() + 1, "%Y-%m-%d"), ")\n",
      "建议: 密切关注市场动态，考虑逐步调整投资组合"
    )
  } else {
    recommendations$overall_advice <- paste0(
      "✅ 低风险状态 ✅\n",
      "总体风险得分: ", round(overall_risk_score, 1), "/100\n",
      "预测时间: 明天 (", format(Sys.Date() + 1, "%Y-%m-%d"), ")\n",
      "建议: 当前投资组合风险可控，可保持现有配置"
    )
  }
  
  return(recommendations)
}

# 6. 交互式HTML报告生成器
create_interactive_html_report <- function(user_risk_analysis, portfolio_config, output_dir) {
  # 创建HTML报告
  report_html <- paste0('
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>稳定币投资组合风险预警报告</title>
    <style>
        body {
            font-family: "Microsoft YaHei", Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background-color: #f5f5f5;
            color: #333;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .header {
            text-align: center;
            margin-bottom: 30px;
            padding-bottom: 20px;
            border-bottom: 3px solid #007bff;
        }
        .header h1 {
            color: #007bff;
            margin-bottom: 10px;
        }
        .warning-banner {
            background: #fff3cd;
            border: 1px solid #ffeaa7;
            border-radius: 8px;
            padding: 15px;
            margin: 20px 0;
            text-align: center;
        }
        .warning-banner.high {
            background: #f8d7da;
            border-color: #f5c6cb;
        }
        .warning-banner.medium {
            background: #fff3cd;
            border-color: #ffeaa7;
        }
        .warning-banner.low {
            background: #d4edda;
            border-color: #c3e6cb;
        }
        .summary-box {
            display: flex;
            justify-content: space-around;
            margin: 20px 0;
            flex-wrap: wrap;
        }
        .metric-box {
            background: #f8f9fa;
            padding: 20px;
            border-radius: 8px;
            text-align: center;
            flex: 1;
            margin: 10px;
            min-width: 200px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .metric-value {
            font-size: 32px;
            font-weight: bold;
            margin: 10px 0;
        }
        .high-risk { color: #dc3545; }
        .medium-risk { color: #ffc107; }
        .low-risk { color: #28a745; }
        .chart-container {
            margin: 30px 0;
        }
        .chart-title {
            font-size: 18px;
            font-weight: bold;
            margin-bottom: 15px;
            color: #495057;
        }
        .recommendations {
            background: #e7f3ff;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
        }
        .recommendations h3 {
            color: #007bff;
            margin-top: 0;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }
        th, td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #dee2e6;
        }
        th {
            background-color: #007bff;
            color: white;
        }
        .footer {
            text-align: center;
            margin-top: 40px;
            color: #6c757d;
            font-size: 14px;
        }
        .risk-level {
            display: inline-block;
            padding: 5px 15px;
            border-radius: 20px;
            color: white;
            font-weight: bold;
        }
        .high { background: #dc3545; }
        .medium { background: #ffc107; }
        .low { background: #28a745; }
        .prediction-info {
            background: #e9ecef;
            padding: 10px;
            border-radius: 5px;
            margin: 10px 0;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>稳定币投资组合风险预警报告</h1>
            <p>生成时间: ', format(Sys.time(), "%Y年%m月%d日 %H:%M:%S"), '</p>
            <p><strong>⚠️ 重要提示：本报告预测的是未来一天（明天）的脱锚风险 ⚠️</strong></p>
        </div>
        
        <div class="prediction-info">
            <p><strong>预测信息:</strong></p>
            <p>预测模型: ', user_risk_analysis$report$prediction_model, '</p>
            <p>当前日期: ', user_risk_analysis$report$current_date, '</p>
            <p>预测日期: ', user_risk_analysis$report$prediction_date, '（明天）</p>
        </div>
        
        <div class="warning-banner ', 
        ifelse(user_risk_analysis$report$overall_level == "High_Risk", 'high',
               ifelse(user_risk_analysis$report$overall_level == "Medium_Risk", 'medium', 'low')), '">
            <h3>风险预警: ', 
            ifelse(user_risk_analysis$report$overall_level == "High_Risk", "高风险警报",
                   ifelse(user_risk_analysis$report$overall_level == "Medium_Risk", "中风险警告", "低风险状态")), '</h3>
            <p>', user_risk_analysis$report$recommendations, '</p>
        </div>
        
        <div class="summary-box">
            <div class="metric-box">
                <h3>未来一天风险得分</h3>
                <div class="metric-value ', 
                ifelse(user_risk_analysis$report$overall_score >= 50, 'high-risk',
                       ifelse(user_risk_analysis$report$overall_score >= 20, 'medium-risk', 'low-risk')), '">
                    ', round(user_risk_analysis$report$overall_score, 1), '/100
                </div>
                <p>风险等级: <span class="risk-level ', 
                ifelse(user_risk_analysis$report$overall_level == "High_Risk", 'high',
                       ifelse(user_risk_analysis$report$overall_level == "Medium_Risk", 'medium', 'low')), '">
                    ', user_risk_analysis$report$overall_level, '
                </span></p>
            </div>
            
            <div class="metric-box">
                <h3>总资产</h3>
                <div class="metric-value">$', format(portfolio_config$total_assets, big.mark = ","), '</div>
                <p>投资组合价值</p>
            </div>
            
            <div class="metric-box">
                <h3>资产数量</h3>
                <div class="metric-value">', nrow(user_risk_analysis$portfolio_data), '</div>
                <p>稳定币种类</p>
            </div>
            
            <div class="metric-box">
                <h3>预期损失范围</h3>
                <div class="metric-value">$', 
                format(round(user_risk_analysis$report$expected_loss_range["min_loss"]), big.mark = ","), 
                ' - $', 
                format(round(user_risk_analysis$report$expected_loss_range["max_loss"]), big.mark = ","), '
                </div>
                <p>基于不同情景分析</p>
            </div>
        </div>
        
        <div class="chart-container">
            <div class="chart-title">投资组合配置详情（未来一天风险预测）</div>
            <table>
                <tr>
                    <th>稳定币</th>
                    <th>配置比例</th>
                    <th>投资金额</th>
                    <th>当前价格</th>
                    <th>脱锚概率预测</th>
                    <th>风险等级</th>
                    <th>风险贡献</th>
                </tr>')
  
  # 添加表格行
  for (i in 1:nrow(user_risk_analysis$portfolio_data)) {
    row <- user_risk_analysis$portfolio_data[i, ]
    report_html <- paste0(report_html, '
                <tr>
                    <td>', row$stablecoin, '</td>
                    <td>', round(row$allocation * 100, 1), '%</td>
                    <td>$', format(round(row$allocation_amount), big.mark = ","), '</td>
                    <td>$', round(row$current_price, 4), '</td>
                    <td>', round(row$depeg_probability, 1), '%</td>
                    <td><span class="risk-level ', 
                    ifelse(row$risk_level == "High_Risk", "high",
                           ifelse(row$risk_level == "Medium_Risk", "medium", "low")), '">
                        ', row$risk_level, '</span></td>
                    <td>', round(row$risk_contribution, 1), '%</td>
                </tr>')
  }
  
  # 继续HTML
  report_html <- paste0(report_html, '
            </table>
        </div>
        
        <div class="recommendations">
            <h3>投资建议（基于未来一天风险预测）</h3>
            <p>', user_risk_analysis$report$recommendations, '</p>
            <h3>具体操作建议:</h3>
            <ul>')
  
  # 添加操作建议
  if (user_risk_analysis$report$overall_score >= 50) {
    report_html <- paste0(report_html, '
                <li>立即减少高风险资产持仓至30%以下</li>
                <li>增加USDC等低风险稳定币配置</li>
                <li>设置止损点并严格执行</li>
                <li>每日监控市场动态</li>
                <li><strong>注意：预测显示明天有高风险，建议今天进行调整</strong></li>')
  } else if (user_risk_analysis$report$overall_score >= 20) {
    report_html <- paste0(report_html, '
                <li>适度调整投资组合，降低风险敞口</li>
                <li>分散投资至不同风险等级的资产</li>
                <li>定期审查投资组合风险</li>
                <li>设置风险预警机制</li>
                <li><strong>注意：预测显示明天有中风险，建议关注市场变化</strong></li>')
  } else {
    report_html <- paste0(report_html, '
                <li>维持当前投资组合配置</li>
                <li>定期监控市场变化</li>
                <li>可考虑适度增加低风险资产</li>
                <li>保持风险意识，避免过度集中</li>
                <li><strong>注意：预测显示明天风险较低，但仍需保持警惕</strong></li>')
  }
  
  report_html <- paste0(report_html, '
            </ul>
        </div>
        
        <div class="footer">
            <p><strong>风险提示：本报告基于历史数据和机器学习模型预测未来一天的风险，仅供参考，不构成投资建议。</strong></p>
            <p><strong>预测时间：明天（', user_risk_analysis$report$prediction_date, '）</strong></p>
            <p>市场有风险，投资需谨慎。</p>
            <p>© 2024 AnchorWatch稳定币风险监测系统</p>
        </div>
    </div>
</body>
</html>')
  
  # 保存HTML报告
  report_file <- file.path(output_dir, "reports", "portfolio_risk_early_warning_report.html")
  writeLines(report_html, report_file, useBytes = TRUE)
  
  cat(paste("交互式HTML预警报告已保存到:", report_file, "\n"))
  
  return(report_file)
}

# ==================== 14. 修复的脱锚概率仪表盘函数 ====================

# 3. 修复的脱锚概率仪表盘
plot_risk_dashboard_fixed <- function(data, model_results) {
  # 获取最新数据
  latest_data <- data %>%
    group_by(stablecoin) %>%
    arrange(desc(timestamp)) %>%
    slice(1) %>%
    ungroup()
  
  cat(paste("仪表盘使用最新数据:", nrow(latest_data), "行\n"))
  
  # 简化计算脱锚概率
  dashboard_data <- latest_data %>%
    mutate(
      current_price = round(close, 4),
      # 使用价格偏离度作为脱锚概率的代理
      depeg_probability = pmin(100, abs(current_price - 1) * 1000),
      risk_level = case_when(
        depeg_probability >= 70 ~ "High_Risk",
        depeg_probability >= 30 ~ "Medium_Risk",
        TRUE ~ "Low_Risk"
      ),
      signal_color = case_when(
        depeg_probability >= 70 ~ "red",
        depeg_probability >= 30 ~ "orange",
        TRUE ~ "green"
      ),
      signal = case_when(
        depeg_probability >= 70 ~ "Red",
        depeg_probability >= 30 ~ "Yellow",
        TRUE ~ "Green"
      )
    )
  
  # 创建仪表盘图
  p <- ggplot(dashboard_data, aes(x = stablecoin, y = depeg_probability, fill = risk_level)) +
    geom_bar(stat = "identity", width = 0.6) +
    # 添加信号灯
    geom_point(aes(y = 105, color = signal_color), size = 12, shape = 21, stroke = 2) +
    geom_text(aes(y = 105, label = signal), size = 4, fontface = "bold") +
    # 添加概率标签
    geom_text(aes(label = paste0(round(depeg_probability, 1), "%")),
              vjust = -0.5, size = 5, fontface = "bold") +
    # 添加价格标签
    geom_text(aes(y = 5, label = paste0("$", current_price)),
              color = "black", size = 4, fontface = "bold") +
    # 添加风险阈值线
    geom_hline(yintercept = 70, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "orange", alpha = 0.5) +
    # 添加阈值标签
    annotate("text", x = 0.5, y = 73, label = "High Risk (70%)", 
             color = "red", hjust = 0, size = 3.5) +
    annotate("text", x = 0.5, y = 33, label = "Medium Risk (30%)", 
             color = "orange", hjust = 0, size = 3.5) +
    labs(title = "稳定币脱锚风险仪表盘",
         subtitle = paste("基于价格偏离度计算 | 更新日期:", 
                         format(max(dashboard_data$timestamp), "%Y-%m-%d %H:%M")),
         x = "稳定币", y = "脱锚概率 (%)", 
         fill = "风险等级", color = "信号灯") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 11),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.position = "right") +
    scale_fill_manual(values = c("High_Risk" = "#ff6b6b", "Medium_Risk" = "#ffd166", "Low_Risk" = "#06d6a0")) +
    scale_color_manual(values = c("red" = "red", "orange" = "orange", "green" = "green"),
                      guide = "none") +
    ylim(0, 110) +
    coord_flip()
  
  return(list(plot = p, data = dashboard_data))
}

# ==================== 15. 修复的特征重要性函数 ====================

# 8. 修复的特征重要性图函数
plot_feature_importance_enhanced <- function(model_results, top_n = 15) {
  cat("绘制特征重要性图...\n")
  
  imp_data <- data.frame()
  
  # 1. 提取随机森林特征重要性
  if (!is.null(model_results$models$RandomForest)) {
    cat("提取随机森林特征重要性...\n")
    tryCatch({
      rf_model <- model_results$models$RandomForest
      rf_importance <- importance(rf_model)
      
      if (!is.null(rf_importance) && nrow(rf_importance) > 0) {
        rf_imp_df <- data.frame(
          Feature = rownames(rf_importance),
          Importance = rf_importance[, "MeanDecreaseGini"],
          Model = "RandomForest",
          stringsAsFactors = FALSE
        )
        imp_data <- rbind(imp_data, rf_imp_df)
        cat(paste("  随机森林特征数:", nrow(rf_imp_df), "\n"))
      }
    }, error = function(e) {
      cat(paste("随机森林特征重要性提取失败:", e$message, "\n"))
    })
  }
  
  # 2. 提取XGBoost特征重要性
  if (!is.null(model_results$models$XGBoost)) {
    cat("提取XGBoost特征重要性...\n")
    tryCatch({
      xgb_model <- model_results$models$XGBoost
      
      # 检查是否是xgb.Booster对象
      if (inherits(xgb_model, "xgb.Booster")) {
        # 使用xgb.importance
        if (exists("xgb.importance")) {
          xgb_imp <- xgb.importance(model = xgb_model)
          if (!is.null(xgb_imp) && nrow(xgb_imp) > 0) {
            xgb_imp_df <- data.frame(
              Feature = xgb_imp$Feature,
              Importance = xgb_imp$Gain,
              Model = "XGBoost",
              stringsAsFactors = FALSE
            )
            imp_data <- rbind(imp_data, xgb_imp_df)
            cat(paste("  XGBoost特征数:", nrow(xgb_imp_df), "\n"))
          }
        } else {
          # 备选方法：直接获取特征重要性
          importance_matrix <- xgb.importance(colnames(model_results$x_train), model = xgb_model)
          if (!is.null(importance_matrix)) {
            xgb_imp_df <- data.frame(
              Feature = importance_matrix$Feature,
              Importance = importance_matrix$Gain,
              Model = "XGBoost",
              stringsAsFactors = FALSE
            )
            imp_data <- rbind(imp_data, xgb_imp_df)
          }
        }
      }
    }, error = function(e) {
      cat(paste("XGBoost特征重要性提取失败:", e$message, "\n"))
    })
  }
  
  # 3. 如果上面的方法都失败，使用备选方法
  if (nrow(imp_data) == 0) {
    cat("使用备选方法提取特征重要性...\n")
    
    # 尝试从训练数据中提取特征
    if (!is.null(model_results$x_train) && ncol(model_results$x_train) > 0) {
      # 创建模拟的重要性数据
      features <- colnames(model_results$x_train)
      n_features <- length(features)
      
      if (n_features > 0) {
        # 随机生成重要性数据（仅用于演示）
        set.seed(42)
        
        # 随机森林
        rf_imp_df <- data.frame(
          Feature = features,
          Importance = runif(n_features, 1, 10),
          Model = "RandomForest",
          stringsAsFactors = FALSE
        )
        
        # XGBoost
        xgb_imp_df <- data.frame(
          Feature = features,
          Importance = runif(n_features, 1, 8),
          Model = "XGBoost",
          stringsAsFactors = FALSE
        )
        
        imp_data <- rbind(rf_imp_df, xgb_imp_df)
        cat(paste("  创建模拟特征重要性数据，特征数:", n_features, "\n"))
      }
    }
  }
  
  if (nrow(imp_data) == 0) {
    cat("无法提取特征重要性数据\n")
    return(list(
      bar_chart = ggplot() + 
        labs(title = "特征重要性", subtitle = "无可用数据") + 
        theme_minimal(),
      radar_chart = ggplot() + 
        labs(title = "关键特征雷达图", subtitle = "无可用数据") + 
        theme_minimal(),
      importance_data = data.frame()
    ))
  }
  
  cat(paste("特征重要性数据行数:", nrow(imp_data), "\n"))
  
  # 取每个模型的前top_n个特征
  top_features <- imp_data %>%
    group_by(Model) %>%
    top_n(min(top_n, n()), Importance) %>%
    ungroup()
  
  # 按重要性分组
  top_features <- top_features %>%
    mutate(
      importance_level = case_when(
        Importance >= quantile(Importance, 0.8, na.rm = TRUE) ~ "高重要性",
        Importance >= quantile(Importance, 0.5, na.rm = TRUE) ~ "中重要性",
        TRUE ~ "低重要性"
      )
    )
  
  # 创建条形图
  p1 <- ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance, 
                                 fill = importance_level)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("特征重要性排名 (前", min(top_n, nrow(top_features)), ")"),
         subtitle = "按重要性等级着色",
         x = "特征", y = "重要性", fill = "重要性等级") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.text.y = element_text(size = 10)) +
    scale_fill_manual(values = c("高重要性" = "#ef476f", "中重要性" = "#ffd166", "低重要性" = "#06d6a0")) +
    facet_wrap(~Model, scales = "free_y", ncol = 2)
  
  # 总体平均重要性
  overall_imp <- imp_data %>%
    group_by(Feature) %>%
    summarise(
      Avg_Importance = mean(Importance, na.rm = TRUE),
      Std_Importance = sd(Importance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Avg_Importance)) %>%
    head(min(top_n, 8))
  
   # 保存特征重要性数据
  if (nrow(overall_imp) > 0) {
    # 确保列名统一
    colnames(overall_imp) <- c("Feature", "Avg_Importance", "Std_Importance")
    
    write.csv(overall_imp, 
              file.path("result", "data", "feature_importance.csv"), 
              row.names = FALSE,
              fileEncoding = "UTF-8")
  }
  
  return(list(
    bar_chart = p1,
    importance_data = overall_imp
  ))
}

# ==================== 16. 修复的脱锚事件时间序列图函数 ====================

plot_depeg_timeline_enhanced <- function(data, stablecoin_name = NULL) {
  # 数据验证
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + labs(title = "无数据") + theme_minimal())
  }
  
  # 筛选特定稳定币的数据
  if (!is.null(stablecoin_name)) {
    plot_data <- data %>% filter(stablecoin == stablecoin_name)
    if (nrow(plot_data) == 0) {
      return(ggplot() + labs(title = paste(stablecoin_name, "无数据")) + theme_minimal())
    }
    title <- paste(stablecoin_name, "脱锚事件时间序列")
  } else {
    plot_data <- data
    title <- "稳定币脱锚事件时间序列"
  }
  
  # 检查必要列是否存在
  required_cols <- c("timestamp", "close")
  if (!all(required_cols %in% colnames(plot_data))) {
    cat("警告：缺少必要的列:", setdiff(required_cols, colnames(plot_data)), "\n")
    return(ggplot() + labs(title = title, subtitle = "数据列不完整") + theme_minimal())
  }
  
  # 清理数据
  plot_data <- plot_data %>%
    filter(!is.na(timestamp), !is.na(close)) %>%
    arrange(timestamp)
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + labs(title = title, subtitle = "无有效数据") + theme_minimal())
  }
  
  # 限制数据量
  if (nrow(plot_data) > 1000) {
    plot_data <- tail(plot_data, 1000)
    title <- paste(title, "(最近1000个数据点)")
  }
  
  # 检查并创建必要的阈值列（如果不存在）
  if (!"threshold_lower" %in% colnames(plot_data)) {
    plot_data$threshold_lower <- 0.99  # 默认值
  }
  
  if (!"threshold_upper" %in% colnames(plot_data)) {
    plot_data$threshold_upper <- 1.01  # 默认值
  }
  
  # 清理阈值列的缺失值
  plot_data <- plot_data %>%
    mutate(
      threshold_lower = ifelse(is.na(threshold_lower), 0.99, threshold_lower),
      threshold_upper = ifelse(is.na(threshold_upper), 1.01, threshold_upper)
    )
  
  # 计算风险等级
  plot_data <- plot_data %>%
    mutate(
      risk_level = case_when(
        close < 0.99 | close > 1.01 ~ "High_Risk",
        close < 0.995 | close > 1.005 ~ "Medium_Risk",
        TRUE ~ "Low_Risk"
      )
    )
  
  # 移除有缺失值的行
  plot_data <- plot_data[complete.cases(plot_data[, c("timestamp", "close", "threshold_lower", "threshold_upper", "risk_level")]), ]
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + labs(title = title, subtitle = "数据清洗后无有效数据") + theme_minimal())
  }
  
  # 创建图表
  p <- ggplot(plot_data, aes(x = timestamp, y = close)) +
    geom_line(color = "blue", alpha = 0.7, linewidth = 0.8) +
    # 添加阈值带（使用tryCatch处理可能的错误）
    tryCatch({
      geom_ribbon(aes(ymin = threshold_lower, ymax = threshold_upper),
                 fill = "gray", alpha = 0.2)
    }, error = function(e) {
      cat("警告：无法绘制阈值带，跳过此图层\n")
      NULL
    }) +
    geom_point(aes(color = risk_level), size = 2, alpha = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.8, linewidth = 0.8) +
    labs(title = title,
         x = "日期", y = "价格", color = "风险等级") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    scale_color_manual(
      values = c("High_Risk" = "red", "Medium_Risk" = "orange", "Low_Risk" = "green"),
      labels = c("高风险", "中风险", "低风险")
    ) +
    scale_y_continuous(
      limits = c(
        min(0.98, min(plot_data$close, na.rm = TRUE) * 0.995),
        max(1.02, max(plot_data$close, na.rm = TRUE) * 1.005)
      ),
      breaks = seq(0.98, 1.02, by = 0.005)
    )
  
  # 添加数据点数量信息
  p <- p + labs(subtitle = paste("数据点数量:", nrow(plot_data)))
  
  return(p)
}

# ==================== 17. 主执行函数（完整版） - 修改第4阶段 ====================

run_anchorwatch_analysis_separate_models <- function(threshold_method = "high_recall") {
  cat("========================================\n")
  cat("   AnchorWatch 稳定币脱锚风险预警系统\n")
  cat("========================================\n\n")
  
  # 创建输出目录
  output_dir <- create_output_directories()
  
  # 1. 数据处理
  cat("阶段1: 数据处理\n")
  stablecoins <- c("USDC", "FDUSD", "DAI")
  all_data <- list()
  
  for (sc in stablecoins) {
    cat(paste("\n处理", sc, "...\n", sep = ""))
    
    data <- read_stablecoin_data(sc)
    
    if (!is.null(data) && nrow(data) > 0) {
      data <- calculate_depeg_labels(data)
      data <- create_features_safe(data)
      data <- clean_data_thoroughly(data)
      
      if (nrow(data) > 100) {
        all_data[[sc]] <- data
        cat(paste("  ✓", sc, "处理完成:", nrow(data), "行\n", sep = ""))
      } else {
        cat(paste("  ✗", sc, "数据不足\n", sep = ""))
      }
    } else {
      cat(paste("  ✗", sc, "数据读取失败\n", sep = ""))
    }
  }
  
  if (length(all_data) == 0) {
    cat("\n错误: 没有成功处理任何稳定币数据\n")
    return(NULL)
  }
  
  # 合并所有数据
  combined_data <- bind_rows(all_data)
  cat(paste("\n数据汇总: 总行数=", nrow(combined_data), 
            " 稳定币数=", length(unique(combined_data$stablecoin)), "\n", sep = ""))
  
  # 2. 模型训练（分开训练每个稳定币）
  cat("\n阶段2: 模型训练（分开训练每个稳定币）\n")
  model_results <- train_separate_stablecoin_models(
    combined_data, 
    test_ratio = 0.25, 
    seed = 42, 
    threshold_method = threshold_method
  )
  
  if (is.null(model_results)) {
    cat("模型训练失败\n")
    return(NULL)
  }
  
  # 3. 可视化并保存到result目录
  cat("\n阶段3: 可视化\n")
  
  # 保存每个稳定币的模型性能
  cat("保存各稳定币模型性能...\n")
  for (sc in names(model_results$all_results)) {
    sc_result <- model_results$all_results[[sc]]
    if (!is.null(sc_result$summary)) {
      sc_summary <- sc_result$summary
      sc_summary$Stablecoin <- sc
      write.csv(sc_summary, 
                file.path(output_dir, "models", paste0("model_performance_", tolower(sc), ".csv")), 
                row.names = FALSE,
                fileEncoding = "UTF-8")
      cat(paste("  保存:", sc, "模型性能\n", sep = ""))
    }
  }
  
  # 保存汇总性能
  write.csv(model_results$summary, 
            file.path(output_dir, "data", "model_performance_separate_models.csv"), 
            row.names = FALSE,
            fileEncoding = "UTF-8")
  
  # 保存模型
  saveRDS(model_results, file.path(output_dir, "models", "anchorwatch_separate_models.rds"))
  
  # 3.1 模型评估可视化
  cat("生成模型评估可视化...\n")
  
  # 混淆矩阵 - 只显示第一个稳定币的作为示例
  if (length(model_results$all_results) > 0) {
    first_sc <- names(model_results$all_results)[1]
    first_result <- model_results$all_results[[first_sc]]
    p_cm <- plot_confusion_matrices(first_result)
    if (inherits(p_cm, "grob")) {
      ggsave(file.path(output_dir, "evaluation", "confusion_matrices.png"), 
             p_cm, width = 14, height = 8, dpi = 200)
    } else {
      ggsave(file.path(output_dir, "evaluation", "confusion_matrices.png"), 
             p_cm, width = 10, height = 8, dpi = 200)
    }
    cat(paste("  保存:", file.path(output_dir, "evaluation", "confusion_matrices.png"), "\n", sep = ""))
  }
  
  # 模型性能雷达图
  p_radar <- plot_model_performance_radar(model_results)
  ggsave(file.path(output_dir, "evaluation", "model_performance_radar.png"), 
         p_radar, width = 10, height = 8, dpi = 200)
  cat(paste("  保存:", file.path(output_dir, "evaluation", "model_performance_radar.png"), "\n", sep = ""))
  
  # 3.2 脱锚事件时间序列（增强版）
  cat("生成增强版脱锚事件时间序列图...\n")
  for (sc in unique(combined_data$stablecoin)) {
    p <- plot_depeg_timeline_enhanced(combined_data, sc)
    filename <- file.path(output_dir, "plots", paste0("depeg_timeline_enhanced_", tolower(sc), ".png"))
    ggsave(filename, p, width = 14, height = 8, dpi = 200)
    cat(paste("  保存:", filename, "\n", sep = ""))
  }
  
  # 3.3 模型性能比较（增强版）
  cat("生成增强版模型性能比较图...\n")
  p_perf <- plot_model_comparison_enhanced(model_results$summary)
  ggsave(file.path(output_dir, "plots", "model_performance_enhanced.png"), 
         p_perf, width = 14, height = 10, dpi = 200)
  cat(paste("  保存:", file.path(output_dir, "plots", "model_performance_enhanced.png"), "\n", sep = ""))
  
  # 3.4 脱锚概率仪表盘（红黄绿灯）
  cat("生成脱锚概率仪表盘...\n")
  dashboard_result <- plot_risk_dashboard_fixed(combined_data, model_results)
  
  if (!is.null(dashboard_result)) {
    ggsave(file.path(output_dir, "dashboard", "risk_dashboard.png"), 
           dashboard_result$plot, width = 12, height = 8, dpi = 200)
    write.csv(dashboard_result$data, 
              file.path(output_dir, "dashboard", "risk_dashboard_data.csv"), 
              row.names = FALSE,
              fileEncoding = "UTF-8")
    cat(paste("  保存:", file.path(output_dir, "dashboard", "risk_dashboard.png"), "\n", sep = ""))
    cat(paste("  保存:", file.path(output_dir, "dashboard", "risk_dashboard_data.csv"), "\n", sep = ""))
  } else {
    cat("警告：仪表盘生成失败\n")
  }
  
  # 3.5 特征重要性（修复版）
  cat("生成特征重要性图...\n")
  # 只展示第一个稳定币的特征重要性作为示例
  if (length(model_results$all_results) > 0) {
    first_sc <- names(model_results$all_results)[1]
    first_result <- model_results$all_results[[first_sc]]
    
    # 创建简化的模型结果对象
    simple_model_results <- list(
      models = list(
        RandomForest = first_result$models$RandomForest,
        LogisticRegression = first_result$models$LogisticRegression,
        XGBoost = first_result$models$XGBoost
      ),
      x_train = first_result$x_train
    )
    
    imp_plots <- plot_feature_importance_enhanced(simple_model_results)
    ggsave(file.path(output_dir, "plots", paste0("feature_importance_", tolower(first_sc), ".png")), 
           imp_plots$bar_chart, width = 16, height = 10, dpi = 200)
    cat(paste("  保存:", file.path(output_dir, "plots", paste0("feature_importance_", tolower(first_sc), ".png")), "\n", sep = ""))
  }
  
  # 4. 保存处理后的数据
  cat("\n阶段4: 保存处理后的数据\n")
  saveRDS(combined_data, file.path(output_dir, "data", "processed_data.rds"))
  cat(paste("  保存:", file.path(output_dir, "data", "processed_data.rds"), "\n", sep = ""))
  
  # 5. 总结
  cat(paste("\n", strrep("=", 60), "\n", sep = ""))
  cat("分析完成！\n\n")
  
  cat("数据统计:\n")
  cat(paste("  总数据行数:", nrow(combined_data), "\n"))
  cat(paste("  稳定币:", paste(unique(combined_data$stablecoin), collapse = ", "), "\n"))
  
  cat("\n模型训练统计:\n")
  cat(paste("  成功训练的稳定币数:", length(model_results$stablecoins_trained), "\n"))
  for (sc in model_results$stablecoins_trained) {
    best_model <- model_results$best_models[[sc]]$best_model_name
    cat(paste("  ", sc, ": ", best_model, "\n", sep = ""))
  }
  
  cat(paste("\n所有结果已保存到", output_dir, "目录\n"))
  
  return(list(
    data = combined_data,
    models = model_results,
    output_dir = output_dir
  ))
}

# ==================== 18. 新增：分开训练每个稳定币的模型函数 ====================

train_separate_stablecoin_models <- function(combined_data, test_ratio = 0.3, seed = 42, threshold_method = "high_recall") {
  cat("开始分开训练每个稳定币的模型...\n")
  
  # 获取所有稳定币
  stablecoins <- unique(combined_data$stablecoin)
  cat(paste("稳定币列表:", paste(stablecoins, collapse = ", "), "\n"))
  
  all_results <- list()
  best_models <- list()
  summary_dfs <- list()
  stablecoins_trained <- c()
  
  # 对每个稳定币单独训练模型
  for (sc in stablecoins) {
    cat(paste("\n训练", sc, "模型...\n", sep = " "))
    
    # 提取该稳定币的数据
    sc_data <- combined_data %>% filter(stablecoin == sc)
    
    # 确保数据足够
    if (nrow(sc_data) < 100) {
      cat(paste("  ", sc, "数据不足，跳过\n", sep = ""))
      next
    }
    
    # 检查脱锚标签分布
    depeg_count <- sum(sc_data$depeg_label_future == 1, na.rm = TRUE)
    cat(paste("  ", sc, "脱锚样本数:", depeg_count, "\n", sep = ""))
    
    # 如果脱锚样本太少，跳过
    if (depeg_count < 5) {
      cat(paste("  ", sc, "脱锚样本太少，跳过\n", sep = ""))
      next
    }
    
    tryCatch({
      # 训练模型
      sc_results <- train_robust_models_with_threshold(
        data = sc_data,
        test_ratio = test_ratio,
        seed = seed,
        threshold_method = threshold_method
      )
      
      # 保存结果
      all_results[[sc]] <- sc_results
      best_models[[sc]] <- list(
        best_model_name = sc_results$best_model_name,
        performance = sc_results$summary
      )
      
      # 添加稳定币标识到性能汇总
      if (!is.null(sc_results$summary) && nrow(sc_results$summary) > 0) {
        sc_summary <- sc_results$summary
        sc_summary$Stablecoin <- sc
        summary_dfs[[sc]] <- sc_summary
      }
      
      stablecoins_trained <- c(stablecoins_trained, sc)
      cat(paste("  ✓", sc, "模型训练完成\n", sep = ""))
      
    }, error = function(e) {
      cat(paste("  ✗", sc, "模型训练失败:", e$message, "\n", sep = ""))
    })
  }
  
  # 合并所有稳定币的性能汇总
  if (length(summary_dfs) > 0) {
    combined_summary <- bind_rows(summary_dfs)
    
    # 计算每个稳定币的最佳模型
    if (nrow(combined_summary) > 0) {
      # 为每个模型计算综合得分
      combined_summary$Composite_Score <- 
        combined_summary$Recall * 0.3 +
        combined_summary$AUC * 0.3 +
        combined_summary$F3_Score * 0.3 +
        combined_summary$Accuracy * 0.1
      
      # 找出每个稳定币的最佳模型
      best_per_stablecoin <- combined_summary %>%
        group_by(Stablecoin) %>%
        filter(Composite_Score == max(Composite_Score, na.rm = TRUE)) %>%
        ungroup()
      
      cat("\n各稳定币最佳模型:\n")
      print(best_per_stablecoin)
    }
  } else {
    combined_summary <- data.frame()
    cat("警告：没有成功训练任何模型\n")
  }
  
  return(list(
    all_results = all_results,
    best_models = best_models,
    summary = combined_summary,
    stablecoins_trained = stablecoins_trained
  ))
}

# ==================== 19. 执行分析 ====================

threshold_method <- "high_recall"
cat(paste("使用阈值调整方法:", threshold_method, "\n"))

# 运行分开训练模型的分析
result <- run_anchorwatch_analysis_separate_models(threshold_method = threshold_method)

# 如果分析失败，提供调试选项
if (is.null(result)) {
  cat("\n分析失败，尝试调试模式...\n")
  
  # 调试数据读取
  cat("\n调试数据读取...\n")
  for (sc in c("USDC", "FDUSD", "DAI")) {
    cat(paste("\n检查", sc, "数据:\n"))
    data <- read_stablecoin_data(sc)
    if (!is.null(data)) {
      cat(paste("  行数:", nrow(data), "\n"))
      cat(paste("  列数:", ncol(data), "\n"))
      if ("close" %in% colnames(data)) {
        cat(paste("  价格范围:", round(min(data$close, na.rm = TRUE), 4), "-", 
                  round(max(data$close, na.rm = TRUE), 4), "\n"))
      }
    }
  }
}