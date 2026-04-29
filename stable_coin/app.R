# Anchor Watch: 稳定币脱锚风险监测平台 - 修复版（与模型代码协调）
# app.R - 主应用程序文件

# 1. 加载必要的R包 ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(caret)
library(pROC)
library(scales)
library(reshape2)

# 2. 加载真实数据和模型 -----------------------------------------------------
cat("正在加载数据和模型...\n")

# 设置数据路径
results_dir <- "result"

# 重新设计的加载函数，与模型代码匹配
load_model_results <- function() {
  loaded_data <- list()
  
  # 1. 加载分开训练的模型结果
  separate_models_path <- file.path(results_dir, "models", "anchorwatch_separate_models.rds")
  if (file.exists(separate_models_path)) {
    loaded_data$separate_models <- readRDS(separate_models_path)
    cat("✓ 已加载分开训练模型结果\n")
    
    # 提取性能汇总
    if (!is.null(loaded_data$separate_models$summary)) {
      loaded_data$model_performance <- loaded_data$separate_models$summary
      cat(paste("✓ 加载模型性能汇总，共", nrow(loaded_data$model_performance), "行\n"))
    }
    
    # 提取每个稳定币的最佳模型信息
    best_models_list <- list()
    if (!is.null(loaded_data$separate_models$all_results)) {
      for (sc in names(loaded_data$separate_models$all_results)) {
        sc_result <- loaded_data$separate_models$all_results[[sc]]
        if (!is.null(sc_result$best_model_name)) {
          # 从汇总数据中提取该稳定币的最佳模型信息
          sc_perf <- loaded_data$model_performance %>%
            filter(Stablecoin == sc, Model == sc_result$best_model_name) %>%
            slice(1)
          
          if (nrow(sc_perf) > 0) {
            best_models_list[[sc]] <- sc_perf
          }
        }
      }
      if (length(best_models_list) > 0) {
        loaded_data$best_models <- bind_rows(best_models_list)
        cat(paste("✓ 已提取", length(best_models_list), "个稳定币的最佳模型\n"))
      }
    }
  
  # 2. 加载处理后的数据
  processed_data_path <- file.path(results_dir, "data", "processed_data.rds")
  if (file.exists(processed_data_path)) {
    loaded_data$processed_data <- readRDS(processed_data_path)
    cat(paste("✓ 已加载处理后的数据，共", nrow(loaded_data$processed_data), "行\n"))
  }
  
  return(loaded_data)
}
  
  # 3. 加载模型性能汇总
  model_performance_path <- file.path(results_dir, "data", "model_performance_separate_models.csv")
  if (file.exists(model_performance_path)) {
    loaded_data$model_performance <- read.csv(model_performance_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    cat(paste("✓ 已加载模型性能汇总数据，共", nrow(loaded_data$model_performance), "行\n"))
    
    # 提取最佳模型信息
    if (nrow(loaded_data$model_performance) > 0) {
      # 找出每个稳定币的最佳模型（基于F3_Score）
      best_models <- loaded_data$model_performance %>%
        group_by(Stablecoin) %>%
        arrange(desc(F3_Score)) %>%
        slice(1) %>%
        ungroup()
      
      loaded_data$best_models <- best_models
      cat(paste("✓ 已识别最佳模型:", paste(best_models$Stablecoin, best_models$Model, sep="=", collapse=", "), "\n"))
    }
  } else {
    cat("✗ 模型性能汇总文件未找到\n")
  }
  
  # 4. 加载特征重要性
  feature_importance_path <- file.path(results_dir, "data", "feature_importance.csv")
  if (file.exists(feature_importance_path)) {
    loaded_data$feature_importance <- read.csv(feature_importance_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    cat(paste("✓ 已加载特征重要性数据\n"))
  } else {
    cat("✗ 特征重要性数据文件未找到，将使用模型数据\n")
  }
  
  # 5. 加载投资组合风险详情（如果存在）
  portfolio_risk_path <- file.path(results_dir, "data", "portfolio_risk_details.csv")
  if (file.exists(portfolio_risk_path)) {
    loaded_data$portfolio_risk <- read.csv(portfolio_risk_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    cat(paste("✓ 已加载投资组合风险数据\n"))
  }
  
  # 6. 加载ROC曲线数据（如果存在）
  roc_data_path <- file.path(results_dir, "evaluation", "roc_data.csv")
  if (file.exists(roc_data_path)) {
    loaded_data$roc_data <- read.csv(roc_data_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    cat(paste("✓ 已加载ROC曲线数据\n"))
  }
  
  return(loaded_data)
}

# 加载所有数据
all_data <- load_model_results()

# 提取数据
processed_data <- all_data$processed_data
separate_models <- all_data$separate_models
model_performance <- all_data$model_performance
best_models <- all_data$best_models
feature_importance <- all_data$feature_importance

# 获取可用的稳定币列表
available_stablecoins <- c("USDC", "FDUSD", "DAI")
if (!is.null(processed_data) && "stablecoin" %in% colnames(processed_data)) {
  available_stablecoins <- unique(processed_data$stablecoin)
  cat(paste("✓ 从数据中获取稳定币列表:", paste(available_stablecoins, collapse = ", "), "\n"))
}

# 3. 辅助函数 --------------------------------------------------------------

# 获取当前稳定币的模型结果
get_current_model_result <- function(stablecoin) {
  if (is.null(separate_models) || is.null(separate_models$all_results[[stablecoin]])) {
    return(NULL)
  }
  return(separate_models$all_results[[stablecoin]])
}

# 获取当前稳定币的最佳模型信息
get_best_model_info <- function(stablecoin) {
  if (is.null(best_models)) return(NULL)
  
  best_info <- best_models %>%
    filter(Stablecoin == stablecoin) %>%
    slice(1)
  
  if (nrow(best_info) == 0) return(NULL)
  return(best_info)
}

# 计算各稳定币的实时风险指标（与模型代码协调）
get_stablecoin_risk_metrics <- function(stablecoin) {
  if (is.null(processed_data)) {
    return(list(
      current_price = 1.0,
      price_deviation = 0,
      depeg_probability = 0.05,
      volatility_7d = 0,
      volume_ratio = 1,
      risk_level = "低风险"
    ))
  }
  
  # 获取该稳定币的最新数据
  data <- processed_data %>%
    filter(stablecoin == !!stablecoin) %>%
    arrange(desc(timestamp)) %>%
    slice_head(n = 1)
  
  if (nrow(data) == 0) {
    return(list(
      current_price = 1.0,
      price_deviation = 0,
      depeg_probability = 0.05,
      volatility_7d = 0,
      volume_ratio = 1,
      risk_level = "低风险"
    ))
  }
  
  # 计算指标
  metrics <- list(
    stablecoin = stablecoin,
    current_price = ifelse("close" %in% colnames(data), round(data$close[1], 4), 1.0),
    price_deviation = ifelse("close" %in% colnames(data), abs(data$close[1] - 1) * 100, 0)
  )
  
  # 获取模型预测的概率（如果可用）
  model_result <- get_current_model_result(stablecoin)
  if (!is.null(model_result) && !is.null(model_result$results)) {
    # 获取最佳模型的预测
    best_model_name <- model_result$best_model_name
    if (!is.null(best_model_name) && !is.null(model_result$results[[best_model_name]])) {
      best_result <- model_result$results[[best_model_name]]
      
      # 使用测试集的平均概率或最新预测
      if (!is.null(best_result$probabilities) && length(best_result$probabilities) > 0) {
        avg_prob <- mean(best_result$probabilities, na.rm = TRUE) * 100
        metrics$depeg_probability <- avg_prob
      }
    }
  }
  
  # 如果模型预测不可用，使用价格偏离估算
  if (is.null(metrics$depeg_probability)) {
    metrics$depeg_probability <- pmin(100, metrics$price_deviation * 10)
  }
  
  # 获取波动率
  if ("volatility_7d" %in% colnames(data) && !is.na(data$volatility_7d[1])) {
    metrics$volatility_7d <- data$volatility_7d[1] * 100
  } else {
    metrics$volatility_7d <- 0
  }
  
  # 获取交易量比率
  if ("volume_ratio" %in% colnames(data) && !is.na(data$volume_ratio[1])) {
    metrics$volume_ratio <- data$volume_ratio[1]
  } else {
    metrics$volume_ratio <- 1
  }
  
  # 确定风险等级（与模型代码一致）
  if (metrics$depeg_probability >= 70) {
    metrics$risk_level <- "高风险"
    metrics$risk_color <- "red"
  } else if (metrics$depeg_probability >= 30) {
    metrics$risk_level <- "中风险"
    metrics$risk_color <- "orange"
  } else {
    metrics$risk_level <- "低风险"
    metrics$risk_color <- "green"
  }
  
  # 获取最佳模型信息
  best_info <- get_best_model_info(stablecoin)
  if (!is.null(best_info)) {
    metrics$best_model <- best_info$Model
    metrics$best_model_f3 <- best_info$F3_Score
  } else {
    metrics$best_model <- "未知"
    metrics$best_model_f3 <- 0
  }
  
  return(metrics)
}

# 获取稳定币历史数据
get_stablecoin_history <- function(stablecoin, days = 30) {
  if (is.null(processed_data)) return(data.frame())
  
  if ("timestamp" %in% colnames(processed_data)) {
    end_date <- max(processed_data$timestamp, na.rm = TRUE)
    start_date <- end_date - days * 24 * 3600
    
    data <- processed_data %>%
      filter(stablecoin == !!stablecoin,
             timestamp >= start_date,
             timestamp <= end_date) %>%
      arrange(timestamp)
    
    return(data)
  } else {
    return(data.frame())
  }
}

# 获取市场数据摘要
get_market_summary <- function(stablecoin) {
  data <- get_stablecoin_history(stablecoin, 30)
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(
      指标 = c("当前价格", "30日价格范围", "平均交易量", "脱锚事件数"),
      数值 = c("N/A", "N/A", "N/A", "N/A"),
      描述 = c("无可用数据", "无可用数据", "无可用数据", "无可用数据"),
      stringsAsFactors = FALSE
    ))
  }
  
  # 计算指标
  current_price <- ifelse("close" %in% colnames(data), tail(data$close, 1), NA)
  price_range <- ifelse("close" %in% colnames(data), 
                       paste0("$", round(min(data$close, na.rm = TRUE), 4), " - $", 
                              round(max(data$close, na.rm = TRUE), 4)), "N/A")
  avg_volume <- ifelse("volume" %in% colnames(data), 
                      mean(data$volume, na.rm = TRUE), NA)
  depeg_events <- ifelse("depeg_label" %in% colnames(data), 
                        sum(data$depeg_label == 1, na.rm = TRUE), NA)
  
  # 格式化
  summary_df <- data.frame(
    指标 = character(),
    数值 = character(),
    描述 = character(),
    stringsAsFactors = FALSE
  )
  
  if (!is.na(current_price)) {
    summary_df <- rbind(summary_df, data.frame(
      指标 = "当前价格",
      数值 = paste0("$", round(current_price, 4)),
      描述 = "最新收盘价"
    ))
  }
  
  if (price_range != "N/A") {
    summary_df <- rbind(summary_df, data.frame(
      指标 = "30日价格范围",
      数值 = price_range,
      描述 = "最近30天价格波动范围"
    ))
  }
  
  if (!is.na(avg_volume)) {
    if (avg_volume > 1e9) {
      volume_str <- paste0("$", round(avg_volume / 1e9, 2), "B")
    } else if (avg_volume > 1e6) {
      volume_str <- paste0("$", round(avg_volume / 1e6, 2), "M")
    } else {
      volume_str <- paste0("$", format(round(avg_volume), big.mark = ","))
    }
    
    summary_df <- rbind(summary_df, data.frame(
      指标 = "日均交易量",
      数值 = volume_str,
      描述 = "30天平均交易量"
    ))
  }
  
  if (!is.na(depeg_events)) {
    summary_df <- rbind(summary_df, data.frame(
      指标 = "脱锚事件数",
      数值 = as.character(depeg_events),
      描述 = "30天内脱锚事件次数"
    ))
  }
  
  # 添加模型信息
  best_info <- get_best_model_info(stablecoin)
  if (!is.null(best_info)) {
    summary_df <- rbind(summary_df, data.frame(
      指标 = "最佳模型",
      数值 = best_info$Model,
      描述 = paste0("F3-score: ", round(best_info$F3_Score, 4))
    ))
  }
  
  return(summary_df)
}

# 计算投资组合风险
calculate_portfolio_risk <- function(allocations, total_assets) {
  if (is.null(allocations) || total_assets <= 0) {
    return(list(
      overall_score = 0,
      overall_level = "低风险",
      components = data.frame(),
      expected_loss = 0,
      var_95 = 0
    ))
  }
  
  # 计算各稳定币的风险贡献
  components <- data.frame()
  total_risk_score <- 0
  
  for (sc in names(allocations)) {
    if (allocations[sc] > 0) {
      metrics <- get_stablecoin_risk_metrics(sc)
      allocation_pct <- allocations[sc] / 100
      allocation_amount <- total_assets * allocation_pct
      
      component_score <- metrics$depeg_probability * allocation_pct
      total_risk_score <- total_risk_score + component_score
      
      components <- rbind(components, data.frame(
        stablecoin = sc,
        allocation_pct = allocation_pct * 100,
        allocation_amount = allocation_amount,
        depeg_probability = metrics$depeg_probability,
        risk_score = component_score,
        risk_level = metrics$risk_level,
        risk_color = metrics$risk_color,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # 确定总体风险等级（与模型代码一致）
  if (total_risk_score >= 50) {
    overall_level <- "高风险"
    overall_color <- "red"
  } else if (total_risk_score >= 20) {
    overall_level <- "中风险"
    overall_color <- "orange"
  } else {
    overall_level <- "低风险"
    overall_color <- "green"
  }
  
  # 计算预期损失和VaR
  expected_loss <- sum(components$allocation_amount * (components$depeg_probability/100) * 0.1, na.rm = TRUE)
  var_95 <- expected_loss * 1.65
  
  return(list(
    overall_score = total_risk_score,
    overall_level = overall_level,
    overall_color = overall_color,
    components = components,
    expected_loss = expected_loss,
    var_95 = var_95
  ))
}

# 4. 用户界面 (UI) -----------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  # 4.1 仪表盘标题
  dashboardHeader(
    title = tags$span(
      icon("shield-alt"), 
      "Anchor Watch 稳定币风险监测平台"
    ),
    titleWidth = 400,
    tags$li(class = "dropdown", 
            tags$span(style = "color: white; padding: 15px; font-size: 14px;", 
                     if (!is.null(processed_data) && "timestamp" %in% colnames(processed_data)) {
                       paste("数据更新:", format(max(processed_data$timestamp, na.rm = TRUE), "%Y-%m-%d"))
                     } else {
                       "数据更新: 未知"
                     })
    )
  ),
  
  # 4.2 侧边栏
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      menuItem("📊 市场总览", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
      menuItem("📈 价格分析", tabName = "price", icon = icon("chart-line")),
      menuItem("🔮 风险预测", tabName = "predict", icon = icon("project-diagram")),
      menuItem("⚖️ 组合优化", tabName = "portfolio", icon = icon("balance-scale")),
      menuItem("📊 模型分析", tabName = "model", icon = icon("chart-bar")),
      menuItem("ℹ️ 关于系统", tabName = "about", icon = icon("info-circle")),
      
      hr(),
      
      # 稳定币选择
      selectInput("stablecoin", 
                  label = tags$span(icon("coins"), "选择稳定币"),
                  choices = available_stablecoins,
                  selected = if (length(available_stablecoins) > 0) available_stablecoins[1] else "USDC"),
      
      # 时间范围选择
      sliderInput("time_range", 
                  label = tags$span(icon("calendar-alt"), "分析周期(天)"),
                  min = 7, max = 180, value = 30, step = 7),
      
      hr(),
      
      # 刷新按钮
      div(style = "padding: 8px;",
          actionButton("refresh", 
                       label = tags$span(icon("sync-alt"), "刷新数据"),
                       class = "btn-primary",
                       style = "width: 90%; font-size: 12px; padding: 6px;")
      )
    )
  ),
  
  # 4.3 主内容区域
  dashboardBody(
    # 自定义CSS样式
    tags$head(
      tags$style(HTML("
        /* 整体样式 */
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        
        /* 卡片样式 */
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          margin-bottom: 15px;
          border: 1px solid #dee2e6;
        }
        
        .box-header {
          border-top-left-radius: 10px;
          border-top-right-radius: 10px;
          padding: 12px 15px;
        }
        
        /* 值盒子样式 */
        .value-box {
          border-radius: 10px;
          margin-bottom: 15px;
          box-shadow: 0 3px 6px rgba(0,0,0,0.1);
        }
        
        /* 风险等级颜色 */
        .risk-low { color: #28a745; font-weight: bold; }
        .risk-medium { color: #ffc107; font-weight: bold; }
        .risk-high { color: #dc3545; font-weight: bold; }
        
        /* 标题样式 */
        h2, h3, h4 {
          color: #343a40;
          font-weight: 600;
        }
        
        h2 {
          margin-top: 5px !important;
          margin-bottom: 20px !important;
        }
        
        /* 表格样式 */
        .dataTables_wrapper {
          font-size: 14px;
        }
        
        /* 混淆矩阵容器 */
        .confusion-matrix-container {
          height: 400px;
          overflow-y: auto;
        }
        
        /* 模型预测结果文本 */
        .model-prediction-text {
          font-size: 14px;
          line-height: 1.6;
          padding: 10px;
          background-color: #f8f9fa;
          border-radius: 5px;
          border: 1px solid #dee2e6;
        }
        
        /* 优化建议样式 */
        .alert-success {
          background-color: #d4edda;
          border-color: #c3e6cb;
          color: #155724;
        }
        
        .alert-warning {
          background-color: #fff3cd;
          border-color: #ffeaa7;
          color: #856404;
        }
        
        .alert-danger {
          background-color: #f8d7da;
          border-color: #f5c6cb;
          color: #721c24;
        }
      "))
    ),
    
    tabItems(
      # === 标签页1: 市场总览 ===
      tabItem(tabName = "dashboard",
              h2("市场数据总览"),
              fluidRow(
                # 关键指标卡片
                valueBoxOutput("price_box", width = 3),
                valueBoxOutput("deviation_box", width = 3),
                valueBoxOutput("volatility_box", width = 3),
                valueBoxOutput("risk_box", width = 3)
              ),
              
              fluidRow(
                # 价格走势与风险分析
                box(
                  title = tags$span(icon("chart-line"), "价格走势与风险分析"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  collapsible = TRUE,
                  plotlyOutput("price_risk_plot", height = "450px")
                ),
                
                # 实时风险仪表
                box(
                  title = tags$span(icon("tachometer-alt"), "实时风险仪表"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("risk_gauge", height = "450px")
                )
              ),
              
              fluidRow(
                # 市场数据摘要
                box(
                  title = tags$span(icon("table"), "市场数据摘要"),
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  DTOutput("market_summary_table")
                )
              )
      ),
      
      # === 标签页2: 价格分析 ===
      tabItem(tabName = "price",
              h2("价格与波动率分析"),
              fluidRow(
                box(
                  title = tags$span(icon("chart-area"), "价格时间序列"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  plotlyOutput("price_timeseries", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = tags$span(icon("wave-square"), "波动率分析"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotlyOutput("volatility_analysis", height = "350px")
                ),
                
                box(
                  title = tags$span(icon("exchange-alt"), "交易量分析"),
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotlyOutput("volume_analysis", height = "350px")
                )
              )
      ),
      
      # === 标签页3: 风险预测 ===
      tabItem(tabName = "predict",
              h2("脱锚风险预测"),
              fluidRow(
                # 模型预测结果
                box(
                  title = tags$span(icon("brain"), "模型预测结果"),
                  status = "success",
                  solidHeader = TRUE,
                  width = 8,
                  collapsible = TRUE,
                  fluidRow(
                    column(6,
                           h4("脱锚概率预测"),
                           div(class = "model-prediction-text",
                               verbatimTextOutput("depeg_prob_output")
                           ),
                           br(),
                           h4("风险等级评估"),
                           div(class = "model-prediction-text",
                               verbatimTextOutput("risk_assessment")
                           )
                    ),
                    column(6,
                           plotlyOutput("probability_gauge", height = "200px"),
                           br(),
                           plotlyOutput("horizon_forecast", height = "200px")
                    )
                  ),
                  plotlyOutput("probability_timeline", height = "250px")
                ),
                
                # 特征重要性
                box(
                  title = tags$span(icon("list-ol"), "特征重要性排名"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  collapsible = TRUE,
                  plotlyOutput("feature_importance_plot", height = "500px")
                )
              ),
              
              fluidRow(
                box(
                  title = tags$span(icon("sliders-h"), "预测参数调整"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  fluidRow(
                    column(3,
                           sliderInput("prediction_horizon", 
                                       "预测时间窗口(小时):",
                                       min = 1, max = 168, value = 24, step = 1)
                    ),
                    column(3,
                           selectInput("prediction_model",
                                       "选择预测模型:",
                                       choices = c("XGBoost", "RandomForest", "LogisticRegression"),
                                       selected = "XGBoost")
                    ),
                    column(3,
                           sliderInput("confidence_level",
                                       "置信水平(%):",
                                       min = 80, max = 99, value = 95, step = 1)
                    ),
                    column(3,
                           br(),
                           actionButton("run_prediction", "运行预测", icon = icon("play"),
                                        class = "btn-primary", width = "100%")
                    )
                  )
                )
              )
      ),
      
      # === 标签页4: 组合优化 ===
      tabItem(tabName = "portfolio",
              h2("投资组合风险分析"),
              fluidRow(
                # 投资组合输入
                box(
                  title = tags$span(icon("calculator"), "投资组合配置"),
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  collapsible = TRUE,
                  numericInput("total_investment", 
                               "总投资金额 (USD):",
                               value = 10000, min = 0, step = 1000,
                               width = "100%"),
                  hr(),
                  h4("稳定币配置比例 (%):"),
                  lapply(available_stablecoins, function(sc) {
                    numericInput(paste0("alloc_", tolower(sc)), 
                                sc,
                                value = round(100/length(available_stablecoins), 1),
                                min = 0, max = 100, step = 0.1,
                                width = "100%")
                  }),
                  hr(),
                  actionButton("calculate_risk", "计算风险", 
                               icon = icon("calculator"),
                               class = "btn-primary", width = "100%"),
                  br(), br(),
                  verbatimTextOutput("allocation_summary")
                ),
                
                # 风险分析结果
                box(
                  title = tags$span(icon("chart-pie"), "风险分析结果"),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 8,
                  collapsible = TRUE,
                  fluidRow(
                    valueBoxOutput("portfolio_risk_score", width = 4),
                    valueBoxOutput("portfolio_expected_loss", width = 4),
                    valueBoxOutput("portfolio_var", width = 4)
                  ),
                  br(),
                  tabsetPanel(
                    id = "portfolio_tabs",
                    type = "tabs",
                    tabPanel("风险雷达图", plotlyOutput("portfolio_radar", height = "300px")),
                    tabPanel("风险贡献", plotlyOutput("risk_contribution", height = "300px")),
                    tabPanel("压力测试", plotlyOutput("stress_test", height = "300px"))
                  ),
                  br(),
                  plotlyOutput("sensitivity_analysis", height = "250px")
                )
              ),
              
              fluidRow(
                box(
                  title = tags$span(icon("lightbulb"), "优化建议"),
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  uiOutput("optimization_recommendations")
                )
              )
      ),
      
      # === 标签页5: 模型分析 ===
      tabItem(tabName = "model",
              h2("模型性能分析"),
              fluidRow(
                box(
                  title = tags$span(icon("trophy"), "模型性能对比"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  plotlyOutput("model_performance_comparison", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = tags$span(icon("th"), "混淆矩阵分析"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  selectInput("confusion_matrix_model",
                              "选择模型:",
                              choices = c("XGBoost", "RandomForest", "LogisticRegression"),
                              selected = "XGBoost"),
                  div(class = "confusion-matrix-container",
                      plotlyOutput("confusion_matrix_plot", height = "350px")
                  )
                ),
                
                box(
                  title = tags$span(icon("chart-curve"), "ROC曲线分析"),
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotlyOutput("roc_curve_plot", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = tags$span(icon("clipboard-list"), "模型详细信息"),
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  verbatimTextOutput("model_details_output")
                )
              )
      ),
      
      # === 标签页6: 关于系统 ===
      tabItem(tabName = "about",
              h2("关于系统"),
              fluidRow(
                box(
                  title = tags$span(icon("info-circle"), "关于 Anchor Watch"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = FALSE,
                  h3("稳定币脱锚风险监测平台"),
                  p("Anchor Watch 是一个基于机器学习的稳定币脱锚风险监测平台，旨在为加密货币投资者提供专业的风险分析和决策支持工具。"),
                  
                  h4("主要功能"),
                  tags$ul(
                    tags$li("📊 实时市场监控与数据分析"),
                    tags$li("🔮 机器学习驱动的脱锚概率预测"),
                    tags$li("⚖️ 个性化投资组合风险评估"),
                    tags$li("📈 多维度可视化分析"),
                    tags$li("🛡️ 风险预警与压力测试")
                  ),
                  
                  h4("支持稳定币"),
                  p(paste("目前支持:", paste(available_stablecoins, collapse = ", "))),
                  
                  h4("模型信息"),
                  p(paste("模型类型: 分开训练（每个稳定币独立模型）")),
                  p(paste("最佳模型（基于F3-score）:")),
                  if (!is.null(best_models)) {
                    tagList(
                      lapply(1:nrow(best_models), function(i) {
                        tags$p(paste0("  • ", best_models$Stablecoin[i], ": ", 
                                     best_models$Model[i], " (F3=", round(best_models$F3_Score[i], 4), ")"))
                      })
                    )
                  },
                  
                  h4("数据信息"),
                  p(paste("数据更新时间:", ifelse(!is.null(processed_data) && "timestamp" %in% colnames(processed_data),
                                                 format(max(processed_data$timestamp, na.rm = TRUE), "%Y-%m-%d"),
                                                 "未知"))),
                  p(paste("数据总量:", ifelse(!is.null(processed_data), nrow(processed_data), "未知"), "行")),
                  
                  h4("系统信息"),
                  p("版本: 3.2.0（与模型代码协调版）"),
                  p("开发团队: 稳不稳"),
                  p("最后更新: 2025-12"),
                  
                  hr(),
                  p(icon("exclamation-triangle"), "风险提示: 本平台提供的分析和预测仅供参考，不构成投资建议。市场有风险，投资需谨慎。")
                )
              )
      )
    )
  )
)

# 5. 服务器逻辑 (Server) -----------------------------------------------------
server <- function(input, output, session) {
  # 添加显示当前稳定币的函数
output$current_stablecoin_for_cm <- renderText({
  input$stablecoin
})

output$current_stablecoin_for_roc <- renderText({
  input$stablecoin
})
  
  # 5.1 反应式数据
  market_data <- reactive({
    req(input$stablecoin, input$time_range)
    
    data <- get_stablecoin_history(input$stablecoin, input$time_range)
    
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }
    
    return(data)
  }) %>% bindEvent(input$refresh, input$stablecoin, input$time_range)
  
  # 5.2 当前风险指标
  current_metrics <- reactive({
    req(input$stablecoin)
    get_stablecoin_risk_metrics(input$stablecoin)
  })
  
  # 5.3 当前模型结果
  current_model_result <- reactive({
    req(input$stablecoin)
    get_current_model_result(input$stablecoin)
  })
  
  # 5.4 值盒子输出
  output$price_box <- renderValueBox({
    metrics <- current_metrics()
    
    valueBox(
      value = sprintf("$%.4f", metrics$current_price),
      subtitle = "当前价格",
      icon = icon("dollar-sign"),
      color = "blue",
      width = 3
    )
  })
  
  output$deviation_box <- renderValueBox({
    metrics <- current_metrics()
    deviation <- metrics$price_deviation
    
    valueBox(
      value = sprintf("%+.3f%%", ifelse(metrics$current_price >= 1, deviation, -deviation)),
      subtitle = "价格偏离",
      icon = icon(ifelse(deviation < 0.5, "check-circle", "exclamation-triangle")),
      color = ifelse(deviation < 0.5, "green", ifelse(deviation < 1, "yellow", "red"))
    )
  })
  
  output$volatility_box <- renderValueBox({
    metrics <- current_metrics()
    volatility <- metrics$volatility_7d
    
    valueBox(
      value = sprintf("%.2f%%", volatility),
      subtitle = "7日波动率",
      icon = icon("wave-square"),
      color = ifelse(volatility < 1, "green", ifelse(volatility < 3, "yellow", "red"))
    )
  })
  
  output$risk_box <- renderValueBox({
    metrics <- current_metrics()
    risk_pct <- metrics$depeg_probability
    
    valueBox(
      value = sprintf("%.1f%%", risk_pct),
      subtitle = "脱锚风险概率",
      icon = icon(ifelse(risk_pct < 10, "shield-alt", 
                        ifelse(risk_pct < 30, "exclamation-triangle", "fire"))),
      color = ifelse(risk_pct < 10, "green", 
                    ifelse(risk_pct < 30, "yellow", "red"))
    )
  })
  
  # 5.5 价格与风险分析图
  output$price_risk_plot <- renderPlotly({
    data <- market_data()
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty() %>%
               layout(title = list(text = "无可用数据", x = 0.5, y = 0.5)))
    }
    
    # 创建主图
    p <- plot_ly(data, x = ~timestamp) %>%
      add_trace(y = ~close, 
                type = 'scatter', 
                mode = 'lines',
                name = '价格',
                line = list(color = '#1f77b4', width = 2),
                yaxis = 'y1',
                hovertemplate = "日期: %{x}<br>价格: $%{y:.4f}<extra></extra>")
    
    # 如果有脱锚概率数据，添加第二个y轴
    if ("depeg_probability" %in% colnames(data)) {
      p <- p %>%
        add_trace(y = ~depeg_probability * 100,
                  type = 'scatter',
                  mode = 'lines',
                  name = '脱锚概率',
                  line = list(color = '#ff7f0e', width = 2, dash = 'dash'),
                  yaxis = 'y2',
                  hovertemplate = "脱锚概率: %{y:.2f}%<extra></extra>")
    }
    
    p <- p %>%
      layout(
        title = list(
          text = paste(input$stablecoin, "价格与风险分析"),
          x = 0.1,
          font = list(size = 16)
        ),
        xaxis = list(
          title = "日期",
          gridcolor = '#e1e5e9'
        ),
        yaxis = list(
          title = "价格 (USD)",
          tickformat = "$,.4f",
          gridcolor = '#e1e5e9',
          side = 'left'
        ),
        yaxis2 = if ("depeg_probability" %in% colnames(data)) {
          list(
            title = "脱锚概率 (%)",
            overlaying = "y",
            side = "right",
            gridcolor = '#e1e5e9',
            range = c(0, 100)
          )
        } else NULL,
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.15,
          x = 0.5,
          xanchor = "center"
        ),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa',
        margin = list(l = 50, r = 50, t = 50, b = 80)
      )
    
    return(p)
  })
  
  # 5.6 风险仪表盘
  output$risk_gauge <- renderPlotly({
    metrics <- current_metrics()
    risk_pct <- metrics$depeg_probability
    
    # 创建仪表盘
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = risk_pct,
      title = list(
        text = paste(input$stablecoin, "风险等级"),
        font = list(size = 16)
      ),
      number = list(
        suffix = "%",
        font = list(size = 24)
      ),
      gauge = list(
        axis = list(
          range = list(0, 100),
          tickwidth = 1,
          tickcolor = "darkblue"
        ),
        bar = list(color = metrics$risk_color),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 10), color = "lightgreen"),
          list(range = c(10, 30), color = "lightyellow"),
          list(range = c(30, 100), color = "lightcoral")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = risk_pct
        )
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 20, t = 50, b = 20),
        font = list(color = "darkblue", family = "Arial")
      )
  })
  
  # 5.7 市场数据摘要表
  output$market_summary_table <- renderDT({
    summary_df <- get_market_summary(input$stablecoin)
    
    datatable(
      summary_df,
      options = list(
        pageLength = 10,
        dom = 't',
        searching = FALSE,
        ordering = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatStyle(
        columns = 1:3,
        fontSize = '14px',
        lineHeight = '20px'
      )
  })
  
  # 5.8 价格时间序列图
  output$price_timeseries <- renderPlotly({
    data <- market_data()
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty())
    }
    
    p <- plot_ly(data, x = ~timestamp) %>%
      add_trace(y = ~close,
                type = 'scatter',
                mode = 'lines',
                name = '收盘价',
                line = list(color = '#3498db', width = 2),
                fill = 'tozeroy',
                fillcolor = 'rgba(52, 152, 219, 0.1)',
                hovertemplate = "日期: %{x}<br>价格: $%{y:.4f}<extra></extra>") %>%
      add_trace(y = 1,
                type = 'scatter',
                mode = 'lines',
                name = '锚定价',
                line = list(color = '#e74c3c', width = 2, dash = 'dash'),
                hovertemplate = "锚定价: $1.0000<extra></extra>") %>%
      layout(
        title = list(
          text = paste(input$stablecoin, "价格走势"),
          x = 0.1,
          font = list(size = 16)
        ),
        xaxis = list(
          title = "日期",
          gridcolor = '#e1e5e9',
          tickformat = "%Y-%m-%d"
        ),
        yaxis = list(
          title = "价格 (USD)",
          tickformat = "$,.4f",
          gridcolor = '#e1e5e9',
          range = c(
            min(0.995, min(data$close, na.rm = TRUE) * 0.999),
            max(1.005, max(data$close, na.rm = TRUE) * 1.001)
          )
        ),
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.25,
          x = 0.5,
          xanchor = "center"
        ),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa',
        margin = list(l = 50, r = 50, t = 50, b = 100)
      )
    
    return(p)
  })
  
  # 5.9 波动率分析图
  output$volatility_analysis <- renderPlotly({
    data <- market_data()
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # 检查是否有波动率数据
    if ("volatility_7d" %in% colnames(data)) {
      volatility_data <- data$volatility_7d * 100
    } else if ("price_volatility" %in% colnames(data)) {
      volatility_data <- data$price_volatility * 100
    } else {
      return(plotly_empty() %>%
               layout(title = list(text = "波动率数据不可用", x = 0.5, y = 0.5)))
    }
    
    p <- plot_ly(data, x = ~timestamp) %>%
      add_trace(y = ~volatility_data,
                type = 'scatter',
                mode = 'lines',
                name = '波动率',
                line = list(color = '#e74c3c', width = 2),
                fill = 'tozeroy',
                fillcolor = 'rgba(231, 76, 60, 0.1)',
                hovertemplate = "日期: %{x}<br>波动率: %{y:.2f}%<extra></extra>") %>%
      layout(
        title = list(
          text = paste(input$stablecoin, "波动率分析"),
          x = 0.1,
          font = list(size = 14)
        ),
        xaxis = list(
          title = "日期",
          gridcolor = '#e1e5e9'
        ),
        yaxis = list(
          title = "波动率 (%)",
          gridcolor = '#e1e5e9'
        ),
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.3,
          x = 0.5,
          xanchor = "center"
        ),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa',
        margin = list(l = 50, r = 50, t = 50, b = 100)
      )
    
    return(p)
  })
  
  # 5.10 交易量分析图
  output$volume_analysis <- renderPlotly({
    data <- market_data()
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty())
    }
    
    if (!"volume" %in% colnames(data)) {
      return(plotly_empty() %>%
               layout(title = list(text = "交易量数据不可用", x = 0.5, y = 0.5)))
    }
    
    p <- plot_ly(data, x = ~timestamp, y = ~volume,
                 type = 'bar',
                 name = '交易量',
                 marker = list(color = '#2ecc71'),
                 hovertemplate = "日期: %{x}<br>交易量: %{y:,.0f}<extra></extra>") %>%
      layout(
        title = list(
          text = paste(input$stablecoin, "交易量分析"),
          x = 0.1,
          font = list(size = 14)
        ),
        xaxis = list(
          title = "日期",
          gridcolor = '#e1e5e9'
        ),
        yaxis = list(
          title = "交易量",
          tickformat = ",.0f",
          gridcolor = '#e1e5e9'
        ),
        hovermode = "x unified",
        showlegend = FALSE,
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa',
        margin = list(l = 50, r = 50, t = 50, b = 80)
      )
    
    return(p)
  })
  
  # 5.11 脱锚概率输出
  output$depeg_prob_output <- renderText({
    metrics <- current_metrics()
    model_result <- current_model_result()
    
    output_text <- paste(
      "稳定币: ", input$stablecoin, "\n",
      "当前价格: $", sprintf("%.4f", metrics$current_price), "\n",
      "价格偏离: ", sprintf("%+.3f%%", 
                           ifelse(metrics$current_price >= 1, 
                                  metrics$price_deviation, 
                                  -metrics$price_deviation)), "\n",
      "脱锚概率: ", sprintf("%.2f%%", metrics$depeg_probability), "\n",
      "7日波动率: ", sprintf("%.2f%%", metrics$volatility_7d), "\n",
      sep = ""
    )
    
    # 添加模型信息
    if (!is.null(model_result)) {
      output_text <- paste(output_text, 
                          "最佳模型: ", metrics$best_model, "\n",
                          "模型F3-score: ", sprintf("%.4f", metrics$best_model_f3),
                          sep = "")
    }
    
    return(output_text)
  })
  
  output$risk_assessment <- renderText({
    metrics <- current_metrics()
    risk_pct <- metrics$depeg_probability
    
    paste(
      "风险等级: ", metrics$risk_level, "\n",
      "风险评分: ", sprintf("%.1f/100", risk_pct), "\n\n",
      if (risk_pct < 10) {
        "✅ 风险极低\n   建议: 可放心持有，适合保守型投资者"
      } else if (risk_pct < 30) {
        "⚠️ 中等风险\n   建议: 保持关注，适合平衡型投资者"
      } else {
        "🚨 高风险警告\n   建议: 谨慎持有，考虑风险对冲"
      },
      sep = ""
    )
  })
  
  # 5.12 概率仪表盘
  output$probability_gauge <- renderPlotly({
    metrics <- current_metrics()
    risk_pct <- metrics$depeg_probability
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = risk_pct,
      title = list(
        text = "脱锚概率",
        font = list(size = 14)
      ),
      number = list(
        suffix = "%",
        font = list(size = 20)
      ),
      gauge = list(
        axis = list(
          range = list(0, 100),
          tickwidth = 1,
          tickcolor = "darkblue"
        ),
        bar = list(
          color = metrics$risk_color
        ),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, 10), color = "rgba(46, 204, 113, 0.3)"),
          list(range = c(10, 30), color = "rgba(243, 156, 18, 0.3)"),
          list(range = c(30, 100), color = "rgba(231, 76, 60, 0.3)")
        )
      ),
      height = 200
    ) %>%
      layout(
        margin = list(l = 30, r = 30, t = 50, b = 30)
      )
  })
  
  # 5.13 预测时间窗口图表
  output$horizon_forecast <- renderPlotly({
    metrics <- current_metrics()
    horizon <- input$prediction_horizon
    
    # 生成预测数据
    hours <- 1:horizon
    # 简单的指数衰减模型
    base_prob <- metrics$depeg_probability
    forecast_probs <- base_prob * (1 + 0.01 * hours)^0.5
    forecast_probs <- pmin(forecast_probs, 100)
    
    forecast_df <- data.frame(
      hour = hours,
      probability = forecast_probs
    )
    
    plot_ly(forecast_df, x = ~hour, y = ~probability,
            type = 'scatter',
            mode = 'lines+markers',
            name = '预测概率',
            line = list(color = '#3498db', width = 2),
            marker = list(size = 6, color = '#3498db'),
            hovertemplate = "未来 %{x} 小时<br>预测概率: %{y:.2f}%<extra></extra>") %>%
      layout(
        title = list(
          text = paste("未来", horizon, "小时预测"),
          font = list(size = 12)
        ),
        xaxis = list(title = "小时"),
        yaxis = list(title = "概率 (%)", range = c(0, max(forecast_probs) * 1.2)),
        showlegend = FALSE,
        margin = list(l = 40, r = 20, t = 40, b = 40)
      )
  })
  
  # 5.14 特征重要性图
output$feature_importance_plot <- renderPlotly({
  if (is.null(feature_importance) || nrow(feature_importance) == 0) {
    # 使用模型数据中的特征
    model_result <- current_model_result()
    if (!is.null(model_result) && !is.null(model_result$feature_cols)) {
      features <- model_result$feature_cols
      importance_df <- data.frame(
        Feature = features,
        Avg_Importance = runif(length(features), 1, 10)  # 改为Avg_Importance
      )
    } else {
      # 使用示例数据（注意列名改为Avg_Importance）
      importance_df <- data.frame(
        Feature = c("price_range", "volatility_7d", "volatility_30d", 
                   "momentum_20", "low", "close_lag1", "close", "month",
                   "high", "open", "price_vs_ma30", "channel_low_20",
                   "channel_high_20", "volume_ratio", "volume_to_rolling"),
        Avg_Importance = c(10.5, 8.2, 7.1, 6.5, 5.8, 5.2, 4.9, 4.5, 4.2, 3.8, 3.5, 3.2, 2.9, 2.5, 2.1)
      )
    }
  } else {
    importance_df <- feature_importance
    
    # 检查列名，如果列名是'Importance'，重命名为'Avg_Importance'
    if ("Importance" %in% colnames(importance_df)) {
      colnames(importance_df)[colnames(importance_df) == "Importance"] <- "Avg_Importance"
    }
    
    # 确保有Feature列
    if (!"Feature" %in% colnames(importance_df) && ncol(importance_df) > 0) {
      # 假设第一列是特征名
      colnames(importance_df)[1] <- "Feature"
    }
  }
  
  # 只显示前15个最重要的特征
  importance_df <- importance_df %>%
    arrange(desc(Avg_Importance)) %>%
    head(15) %>%
    arrange(Avg_Importance)
  
  # 创建颜色映射
  colors <- ifelse(importance_df$Avg_Importance >= quantile(importance_df$Avg_Importance, 0.8, na.rm = TRUE), "#ef476f",
                  ifelse(importance_df$Avg_Importance >= quantile(importance_df$Avg_Importance, 0.5, na.rm = TRUE), "#ffd166", "#06d6a0"))
  
  p <- plot_ly(
    importance_df,
    y = ~reorder(Feature, Avg_Importance),
    x = ~Avg_Importance,
    type = "bar",
    orientation = "h",
    marker = list(color = colors),
    text = ~sprintf("%.2f", Avg_Importance),
    textposition = "outside",
    hovertemplate = "%{y}: %{x:.3f}<extra></extra>"
  ) %>%
    layout(
      title = list(
        text = paste(input$stablecoin, "特征重要性排名"),
        x = 0.1,
        font = list(size = 14)
      ),
      xaxis = list(
        title = "重要性",
        gridcolor = '#e1e5e9'
      ),
      yaxis = list(
        title = "",
        gridcolor = '#e1e5e9'
      ),
      showlegend = FALSE,
      plot_bgcolor = '#f8f9fa',
      paper_bgcolor = '#f8f9fa',
      margin = list(l = 150, r = 20, t = 50, b = 50)
    )
  
  return(p)
})
  
  # 5.15 概率时间序列图
  output$probability_timeline <- renderPlotly({
    data <- market_data()
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # 检查是否有脱锚概率数据
    if ("depeg_probability" %in% colnames(data)) {
      p <- plot_ly(data, x = ~timestamp) %>%
        add_trace(y = ~depeg_probability * 100,
                  type = 'scatter',
                  mode = 'lines',
                  name = '脱锚概率',
                  line = list(color = '#ff7f0e', width = 2),
                  fill = 'tozeroy',
                  fillcolor = 'rgba(255, 127, 14, 0.1)',
                  hovertemplate = "日期: %{x}<br>概率: %{y:.2f}%<extra></extra>")
      
      # 添加风险阈值线
      p <- p %>%
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(data$timestamp, na.rm = TRUE),
              x1 = max(data$timestamp, na.rm = TRUE),
              y0 = 10,
              y1 = 10,
              line = list(color = "green", dash = "dash", width = 1)
            ),
            list(
              type = "line",
              x0 = min(data$timestamp, na.rm = TRUE),
              x1 = max(data$timestamp, na.rm = TRUE),
              y0 = 30,
              y1 = 30,
              line = list(color = "orange", dash = "dash", width = 1)
            ),
            list(
              type = "line",
              x0 = min(data$timestamp, na.rm = TRUE),
              x1 = max(data$timestamp, na.rm = TRUE),
              y0 = 70,
              y1 = 70,
              line = list(color = "red", dash = "dash", width = 1)
            )
          ),
          title = list(
            text = "脱锚概率时间序列",
            x = 0.1,
            font = list(size = 14)
          ),
          xaxis = list(
            title = "日期",
            gridcolor = '#e1e5e9'
          ),
          yaxis = list(
            title = "脱锚概率 (%)",
            gridcolor = '#e1e5e9',
            range = c(0, 100)
          ),
          hovermode = "x unified",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            y = -0.15,
            x = 0.5,
            xanchor = "center"
          ),
          plot_bgcolor = '#f8f9fa',
          paper_bgcolor = '#f8f9fa',
          margin = list(l = 50, r = 50, t = 50, b = 80)
        )
    } else {
      p <- plot_ly() %>%
        layout(
          title = list(
            text = "脱锚概率数据不可用",
            x = 0.5,
            y = 0.5
          )
        )
    }
    
    return(p)
  })
  
  # 5.16 投资组合风险计算
  portfolio_results <- reactiveValues(
    risk_score = 0,
    risk_level = "低风险",
    risk_color = "green",
    components = data.frame(),
    expected_loss = 0,
    var_95 = 0
  )
  
  observeEvent(input$calculate_risk, {
    # 收集分配比例
    allocations <- sapply(available_stablecoins, function(sc) {
      input_name <- paste0("alloc_", tolower(sc))
      if (!is.null(input[[input_name]])) {
        return(input[[input_name]])
      } else {
        return(0)
      }
    })
    
    names(allocations) <- available_stablecoins
    
    # 计算投资组合风险
    total_assets <- input$total_investment
    risk_result <- calculate_portfolio_risk(allocations, total_assets)
    
    # 更新反应式值
    portfolio_results$risk_score <- risk_result$overall_score
    portfolio_results$risk_level <- risk_result$overall_level
    portfolio_results$risk_color <- risk_result$overall_color
    portfolio_results$components <- risk_result$components
    portfolio_results$expected_loss <- risk_result$expected_loss
    portfolio_results$var_95 <- risk_result$var_95
    
    showNotification("投资组合风险计算完成!", type = "message", duration = 3)
  })
  
  # 5.17 分配比例摘要
  output$allocation_summary <- renderText({
    allocations <- sapply(available_stablecoins, function(sc) {
      input_name <- paste0("alloc_", tolower(sc))
      if (!is.null(input[[input_name]])) {
        return(input[[input_name]])
      } else {
        return(0)
      }
    })
    
    total <- sum(allocations)
    
    result <- paste0(
      "分配比例汇总:\n",
      "---------------\n"
    )
    
    for (sc in available_stablecoins) {
      result <- paste0(result, sprintf("%-6s: %6.1f%%\n", sc, allocations[sc]))
    }
    
    result <- paste0(result, 
      "---------------\n",
      sprintf("总计:   %6.1f%%\n", total)
    )
    
    if (abs(total - 100) > 0.1) {
      result <- paste0(result, "\n⚠️ 警告: 分配比例总和应为100%\n")
    }
    
    return(result)
  })
  
  # 5.18 投资组合风险值盒子
  output$portfolio_risk_score <- renderValueBox({
    valueBox(
      value = sprintf("%.1f/100", portfolio_results$risk_score),
      subtitle = "风险评分",
      icon = icon(ifelse(portfolio_results$risk_score < 20, "shield-alt",
                        ifelse(portfolio_results$risk_score < 50, "exclamation-triangle", "fire"))),
      color = ifelse(portfolio_results$risk_score < 20, "green",
                    ifelse(portfolio_results$risk_score < 50, "yellow", "red"))
    )
  })
  
  output$portfolio_expected_loss <- renderValueBox({
    valueBox(
      value = sprintf("$%s", format(round(portfolio_results$expected_loss), big.mark = ",")),
      subtitle = "预期损失",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$portfolio_var <- renderValueBox({
    valueBox(
      value = sprintf("$%s", format(round(portfolio_results$var_95), big.mark = ",")),
      subtitle = "95% VaR",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  # 5.19 投资组合风险雷达图
  output$portfolio_radar <- renderPlotly({
    if (nrow(portfolio_results$components) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "请先计算投资组合风险",
                   x = 0.5,
                   y = 0.5
                 )
               ))
    }
    
    # 准备雷达图数据
    data <- portfolio_results$components
    
    # 创建雷达图
    plot_ly(
      type = 'scatterpolar',
      r = c(data$depeg_probability, data$depeg_probability[1]),
      theta = c(data$stablecoin, data$stablecoin[1]),
      fill = 'toself',
      fillcolor = 'rgba(52, 152, 219, 0.3)',
      line = list(color = 'rgba(52, 152, 219, 0.8)', width = 2),
      marker = list(size = 8),
      name = '脱锚概率',
      hovertemplate = "%{theta}: %{r:.1f}%<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(c(data$depeg_probability, 50), na.rm = TRUE) * 1.2),
            tickfont = list(size = 10),
            tickformat = ".0f",
            gridcolor = '#e1e5e9'
          ),
          angularaxis = list(
            direction = "clockwise",
            rotation = 90,
            gridcolor = '#e1e5e9'
          ),
          bgcolor = '#f8f9fa'
        ),
        showlegend = FALSE,
        title = list(
          text = "稳定币风险分布雷达图",
          x = 0.1,
          font = list(size = 14)
        ),
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })
  
  # 5.20 风险贡献图
  output$risk_contribution <- renderPlotly({
    if (nrow(portfolio_results$components) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "请先计算投资组合风险",
                   x = 0.5,
                   y = 0.5
                 )
               ))
    }
    
    data <- portfolio_results$components %>%
      mutate(risk_contribution = risk_score / sum(risk_score) * 100)
    
    # 创建饼图
    plot_ly(
      data,
      labels = ~stablecoin,
      values = ~risk_contribution,
      type = 'pie',
      hole = 0.4,
      textinfo = 'label+percent',
      hoverinfo = 'label+percent+value',
      marker = list(
        colors = ~risk_color,
        line = list(color = '#FFFFFF', width = 2)
      ),
      hovertemplate = "%{label}<br>风险贡献: %{percent}<br>脱锚概率: %{text}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = "风险贡献分布",
          x = 0.5,
          font = list(size = 14)
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.1,
          x = 0.5,
          xanchor = "center"
        )
      )
  })
  
  # 5.21 压力测试图
  output$stress_test <- renderPlotly({
    if (nrow(portfolio_results$components) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "请先计算投资组合风险",
                   x = 0.5,
                   y = 0.5
                 )
               ))
    }
    
    # 定义压力测试情景
    scenarios <- data.frame(
      scenario = c("基准情景", "轻度脱锚(5%)", "中度脱锚(10%)", "重度脱锚(20%)", "极端脱锚(50%)"),
      loss_rate = c(0, 0.05, 0.10, 0.20, 0.50),
      probability = c(0.7, 0.2, 0.07, 0.02, 0.01),
      color = c('#2ecc71', '#f1c40f', '#e67e22', '#e74c3c', '#8e44ad')
    )
    
    # 计算损失
    total_assets <- input$total_investment
    scenarios <- scenarios %>%
      mutate(
        expected_loss = total_assets * (portfolio_results$risk_score/100) * loss_rate * probability,
        label = paste0(
          scenario, "\n",
          "损失率: ", loss_rate * 100, "%\n",
          "概率: ", probability * 100, "%\n",
          "预期损失: $", format(round(expected_loss), big.mark = ",")
        )
      )
    
    # 创建压力测试图
    plot_ly(
      scenarios,
      x = ~scenario,
      y = ~expected_loss,
      type = 'bar',
      marker = list(color = ~color),
      text = ~paste0("$", format(round(expected_loss), big.mark = ",")),
      textposition = 'outside',
      hoverinfo = 'text',
      hovertext = ~label
    ) %>%
      layout(
        title = list(
          text = "压力测试情景分析",
          x = 0.1,
          font = list(size = 14)
        ),
        xaxis = list(title = "情景"),
        yaxis = list(title = "预期损失 ($)", tickformat = "$,.0f"),
        showlegend = FALSE,
        margin = list(l = 50, r = 50, t = 50, b = 80)
      )
  })
  
  # 5.22 敏感度分析图
  output$sensitivity_analysis <- renderPlotly({
    if (nrow(portfolio_results$components) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "请先计算投资组合风险",
                   x = 0.5,
                   y = 0.5
                 )
               ))
    }
    
    # 生成敏感度分析数据
    shock_levels <- seq(-0.2, 0.2, by = 0.05)
    
    # 计算不同冲击下的潜在损失
    total_assets <- input$total_investment
    base_risk <- portfolio_results$risk_score
    
    losses <- total_assets * (base_risk/100) * (0.1 + 0.5 * abs(shock_levels))
    
    sensitivity_df <- data.frame(
      market_shock = shock_levels * 100,
      potential_loss = losses,
      potential_loss_pct = (losses / total_assets) * 100
    )
    
    # 创建双Y轴图表
    plot_ly(sensitivity_df, x = ~market_shock) %>%
      add_trace(y = ~potential_loss, name = "绝对损失 (USD)", 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = "darkred", width = 3),
                marker = list(size = 8, color = "darkred"),
                yaxis = "y1",
                hovertemplate = "市场冲击: %{x}%<br>潜在损失: $%{y:,.0f}<extra></extra>") %>%
      add_trace(y = ~potential_loss_pct, name = "相对损失 (%)", 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = "orange", width = 3, dash = "dash"),
                marker = list(size = 8, color = "orange"),
                yaxis = "y2",
                hovertemplate = "市场冲击: %{x}%<br>损失比例: %{y:.2f}%<extra></extra>") %>%
      layout(
        title = list(
          text = "市场冲击下的潜在损失敏感度分析",
          x = 0.1,
          font = list(size = 12)
        ),
        xaxis = list(
          title = "市场冲击 (%)",
          tickmode = "array",
          tickvals = seq(-20, 20, by = 5)
        ),
        yaxis = list(
          title = "潜在损失 (USD)",
          tickformat = "$,",
          side = "left"
        ),
        yaxis2 = list(
          title = "损失比例 (%)",
          tickformat = ".1f",
          overlaying = "y",
          side = "right"
        ),
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.15,
          x = 0.5,
          xanchor = "center"
        ),
        margin = list(l = 50, r = 50, t = 50, b = 80)
      )
  })
  
  # 5.23 混淆矩阵图 - 添加稳定币选择
output$confusion_matrix_plot <- renderPlotly({
  # 获取用户选择的稳定币
  stablecoin_choice <- input$stablecoin  # 使用当前选择的稳定币
  
  model_result <- get_current_model_result(stablecoin_choice)
  if (is.null(model_result)) {
    return(plotly_empty() %>%
             layout(
               title = list(
                 text = paste("模型数据未加载"),
                 x = 0.5,
                 y = 0.5
               )
             ))
  }
  
  # 根据选择的模型显示对应的混淆矩阵
  model_name <- input$confusion_matrix_model
  
  # 获取混淆矩阵数据
  if (!is.null(model_result$results[[model_name]])) {
    result <- model_result$results[[model_name]]
    
    if (!is.null(result$confusion_matrix)) {
      cm <- result$confusion_matrix$table
      
      # 创建热图
      plot_ly(
        x = colnames(cm),
        y = rownames(cm),
        z = cm,
        type = "heatmap",
        colorscale = list(
          c(0, "white"),
          c(1, "#3498db")
        ),
        colorbar = list(
          title = "数量",
          len = 0.5
        ),
        hovertemplate = "实际: %{y}<br>预测: %{x}<br>数量: %{z}<extra></extra>"
      ) %>%
        layout(
          title = list(
            text = paste(stablecoin_choice, "-", model_name, "混淆矩阵"),
            x = 0.1,
            font = list(size = 14)
          ),
          xaxis = list(
            title = "预测类别",
            gridcolor = '#e1e5e9'
          ),
          yaxis = list(
            title = "实际类别",
            gridcolor = '#e1e5e9'
          ),
          showlegend = FALSE,
          plot_bgcolor = '#f8f9fa',
          paper_bgcolor = '#f8f9fa',
          margin = list(l = 50, r = 50, t = 50, b = 50)
        )
    } else {
      plotly_empty() %>%
        layout(
          title = list(
            text = paste(model_name, "混淆矩阵数据不可用"),
            x = 0.5,
            y = 0.5
          )
        )
    }
  } else {
    plotly_empty() %>%
      layout(
        title = list(
          text = paste(model_name, "模型结果不可用"),
          x = 0.5,
          y = 0.5
        )
      )
  }
})

# 5.24 ROC曲线图 - 修改为可选择稳定币
output$roc_curve_plot <- renderPlotly({
  # 获取用户选择的稳定币
  stablecoin_choice <- input$stablecoin  # 使用当前选择的稳定币
  
  model_result <- get_current_model_result(stablecoin_choice)
  if (is.null(model_result)) {
    return(plotly_empty() %>%
             layout(
               title = list(
                 text = paste("模型数据未加载"),
                 x = 0.5,
                 y = 0.5
               )
             ))
  }
  
  # 创建ROC曲线数据
  roc_data <- data.frame()
  
  # 收集所有可用的ROC数据
  for (model_name in c("XGBoost", "RandomForest", "LogisticRegression")) {
    if (!is.null(model_result$results[[model_name]]) && 
        !is.null(model_result$results[[model_name]]$auc)) {
      
      result <- model_result$results[[model_name]]
      auc_value <- result$auc
      
      # 如果模型有ROC对象，直接使用
      if (!is.null(result$roc)) {
        roc_obj <- result$roc
        temp_df <- data.frame(
          FPR = 1 - roc_obj$specificities,
          TPR = roc_obj$sensitivities,
          Model = paste0(model_name, " (AUC=", round(auc_value, 3), ")")
        )
      } else {
        # 模拟ROC曲线数据
        fpr <- seq(0, 1, 0.01)
        
        # 根据AUC生成合理的ROC曲线
        if (auc_value > 0.5) {
          tpr <- fpr^(1/(2 * auc_value))
        } else {
          tpr <- fpr
        }
        
        temp_df <- data.frame(
          FPR = fpr,
          TPR = tpr,
          Model = paste0(model_name, " (AUC=", round(auc_value, 3), ")")
        )
      }
      
      roc_data <- rbind(roc_data, temp_df)
    }
  }
  
  if (nrow(roc_data) == 0) {
    return(plotly_empty() %>%
             layout(
               title = list(
                 text = "ROC曲线数据不可用",
                 x = 0.5,
                 y = 0.5
               )
             ))
  }
  
  # 创建ROC曲线图
  colors <- c("XGBoost" = "#e74c3c", 
              "RandomForest" = "#3498db", 
              "LogisticRegression" = "#2ecc71")
  
  p <- plot_ly() %>%
    # 添加随机猜测线
    add_trace(
      x = c(0, 1),
      y = c(0, 1),
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'gray', dash = 'dash', width = 1),
      name = '随机猜测',
      showlegend = FALSE,
      hoverinfo = 'none'
    )
  
  # 添加各模型的ROC曲线
  for (model in unique(roc_data$Model)) {
    model_data <- roc_data[roc_data$Model == model, ]
    
    # 提取模型名称
    model_name_simple <- gsub(" \\(AUC=.*\\)", "", model)
    
    p <- p %>%
      add_trace(
        data = model_data,
        x = ~FPR,
        y = ~TPR,
        type = 'scatter',
        mode = 'lines',
        name = model,
        line = list(color = colors[model_name_simple], width = 3),
        hoverinfo = 'text',
        text = ~paste("模型:", model)
      )
  }
  
p <- p %>%
  layout(
    title = paste(stablecoin_choice, "ROC曲线对比"),
    xaxis = list(
      title = "假阳性率 (FPR)",
      titlefont = list(size = 9),  # 直接设置标题字体大小
      range = c(0, 1),
      gridcolor = '#e1e5e9',
      tickfont = list(size = 8)
    ),
    yaxis = list(
      title = "真阳性率 (TPR)",
      titlefont = list(size = 9),  # 直接设置标题字体大小
      range = c(0, 1),
      gridcolor = '#e1e5e9',
      tickfont = list(size = 11)
    ),
    legend = list(
      orientation = "h",
      y = -0.3,  # 适当调整位置
      x = 0.5,
      font = list(size = 10)
    ),
    margin = list(l = 50, r = 50, t = 50, b = 100)
  )

  return(p)
})
  

  
# 5.25 模型性能对比图 - 修改为全局模型对比
output$model_performance_comparison <- renderPlotly({
  req(model_performance)
  
  # 检查数据是否有效
  if (nrow(model_performance) == 0) {
    return(plotly_empty() %>%
             layout(
               title = list(
                 text = "无模型性能数据",
                 x = 0.5,
                 y = 0.5
               )
             ))
  }
  
  # 检查必需的列是否存在
  required_cols <- c("Model", "Accuracy", "Recall", "Sensitivity", "Specificity", "AUC", "F3_Score")
  missing_cols <- setdiff(required_cols, colnames(model_performance))
  
  if (length(missing_cols) > 0) {
    # 尝试使用可用列
    available_metrics <- intersect(required_cols, colnames(model_performance))
    if (length(available_metrics) < 2) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = paste("数据列不足，缺少:", paste(missing_cols, collapse = ", ")),
                   x = 0.5,
                   y = 0.5
                 )
               ))
    }
  }
  
  # 计算每个模型的平均性能
  tryCatch({
    model_summary <- model_performance %>%
      group_by(Model) %>%
      summarise(
        Accuracy = mean(Accuracy, na.rm = TRUE),
        Recall = mean(Recall, na.rm = TRUE),
        Sensitivity = mean(Sensitivity, na.rm = TRUE),
        Specificity = mean(Specificity, na.rm = TRUE),
        AUC = mean(AUC, na.rm = TRUE),
        F3_Score = mean(F3_Score, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # 转换为长格式用于绘图
    perf_long <- model_summary %>%
      pivot_longer(
        cols = -Model,
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      mutate(
        Value = as.numeric(Value),
        Model = factor(Model, levels = c("LogisticRegression", "RandomForest", "XGBoost"))
      )
    
    # 确保所有模型都有数据
    all_models <- c("LogisticRegression", "RandomForest", "XGBoost")
    missing_models <- setdiff(all_models, unique(perf_long$Model))
    
    if (length(missing_models) > 0) {
      # 为缺失的模型添加占位数据
      for (model in missing_models) {
        placeholder <- data.frame(
          Model = model,
          Metric = unique(perf_long$Metric),
          Value = 0.5,
          stringsAsFactors = FALSE
        )
        perf_long <- rbind(perf_long, placeholder)
      }
    }
    
    # 创建分组柱状图
    colors <- c("Accuracy" = "#3498db",
                "Recall" = "#2ecc71",
                "Sensitivity" = "#e74c3c",
                "Specificity" = "#f39c12",
                "AUC" = "#9b59b6",
                "F3_Score" = "#1abc9c")
    
    p <- plot_ly(
      data = perf_long,
      x = ~Model,
      y = ~Value,
      color = ~Metric,
      colors = colors,
      type = "bar",
      text = ~sprintf("%.3f", Value),
      textposition = "outside",
      hovertemplate = paste(
        "模型: %{x}<br>",
        "指标: %{fullData.name}<br>",
        "值: %{y:.3f}<br>",
        "<extra></extra>"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = "三种模型性能对比（跨所有稳定币平均）",
          x = 0.1,
          font = list(size = 16),
          # 添加标题下边距
          pad = list(b = 10)
        ),
        xaxis = list(
          title = "模型",
          gridcolor = '#e1e5e9',
          categoryorder = "array",
          categoryarray = c("LogisticRegression", "RandomForest", "XGBoost"),
          # 调整x轴标题位置
          title_standoff = 15
        ),
        yaxis = list(
          title = list(
            text = "分数",
            # 调整y轴标题位置，避免与图例重叠
            standoff = 5
          ),
          range = c(0, 1),
          gridcolor = '#e1e5e9',
          # 调整刻度标签与轴线的距离
          ticklen = 5,
          tickwidth = 1,
          tickfont = list(size = 10)
        ),
        barmode = "group",
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          # 将图例移到更下方，避免与y轴标题重叠
          y = -0.5,  
          x = 0.5,
          xanchor = "center",
          # 调整图例项间距
          itemwidth = 30,
          itemsizing = "constant",
          # 调整图例字体大小
          font = list(size = 10),
          # 调整图例与图表的距离
          yanchor = "top",
          traceorder = "normal",
          # 减少图例高度
          itemclick = "toggle",
          itemdoubleclick = "toggleothers"
        ),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa',
        # 增加底部边距，为图例留出更多空间
        margin = list(l = 60, r = 50, t = 80, b = 150),  # b从100调整为150
        # 添加整体内边距
        pad = list(b = 20, l = 10, r = 10, t = 10)
      )
    
    return(p)
    
  }, error = function(e) {
    # 如果出现错误，显示错误信息
    error_message <- paste("图表生成错误:", e$message)
    
    return(plotly_empty() %>%
             layout(
               title = list(
                 text = error_message,
                 x = 0.5,
                 y = 0.5,
                 font = list(color = "red", size = 14)
               )
             ))
  })
})

# 5.26 模型详细信息输出 - 修改为全局模型信息
output$model_details_output <- renderPrint({
  if (is.null(model_performance) || nrow(model_performance) == 0) {
    cat("=== 模型详细信息 ===\n\n")
    cat("模型性能数据未加载\n")
    return()
  }
  
  cat("=== 三种模型全局性能对比 ===\n\n")
  
  # 计算每个模型的统计摘要
  model_summary <- model_performance %>%
    group_by(Model) %>%
    summarise(
      平均准确率 = mean(Accuracy, na.rm = TRUE),
      平均召回率 = mean(Recall, na.rm = TRUE),
      平均敏感度 = mean(Sensitivity, na.rm = TRUE),
      平均特异度 = mean(Specificity, na.rm = TRUE),
      平均AUC = mean(AUC, na.rm = TRUE),
      平均F3分数 = mean(F3_Score, na.rm = TRUE),
      样本数 = n(),
      .groups = 'drop'
    )
  
  cat("1. 各模型性能汇总（跨所有稳定币）:\n")
  print(as.data.frame(model_summary))
  
  cat("\n2. 各稳定币表现最佳的模型:\n")
  if (!is.null(best_models)) {
    for (i in 1:nrow(best_models)) {
      cat(paste("  • ", best_models$Stablecoin[i], ": ", best_models$Model[i], 
                " (F3=", round(best_models$F3_Score[i], 4), ")\n", sep = ""))
    }
  }
  
  cat("\n3. 模型性能排名（基于F3分数）:\n")
  if (!is.null(best_models)) {
    # 计算每个模型的平均F3分数
    model_f3 <- model_performance %>%
      group_by(Model) %>%
      summarise(平均F3分数 = mean(F3_Score, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(平均F3分数))
    
    for (i in 1:nrow(model_f3)) {
      cat(paste("  ", i, ". ", model_f3$Model[i], 
                " (平均F3=", round(model_f3$平均F3分数[i], 4), ")\n", sep = ""))
    }
  }
  
  cat("\n4. 模型选择建议:\n")
  if (!is.null(model_summary)) {
    # 找出最佳模型
    best_model_f3 <- model_summary %>%
      arrange(desc(平均F3分数)) %>%
      slice(1)
    
    best_model_auc <- model_summary %>%
      arrange(desc(平均AUC)) %>%
      slice(1)
    
    best_model_recall <- model_summary %>%
      arrange(desc(平均召回率)) %>%
      slice(1)
    
    cat(paste("  • 总体最佳（F3分数）: ", best_model_f3$Model, "\n", sep = ""))
    cat(paste("  • 最佳AUC: ", best_model_auc$Model, "\n", sep = ""))
    cat(paste("  • 最佳召回率: ", best_model_recall$Model, "\n", sep = ""))
    
    cat("\n  • 模型选择建议:\n")
    cat("     如果优先考虑召回率（避免漏报脱锚事件）: ", best_model_recall$Model, "\n")
    cat("     如果优先考虑AUC（整体区分能力）: ", best_model_auc$Model, "\n")
    cat("     如果综合考虑（F3分数）: ", best_model_f3$Model, "\n")
  }
  
  cat("\n5. 数据统计:\n")
  cat(paste("  • 总稳定币数量: ", length(available_stablecoins), "\n"))
  cat(paste("  • 每个模型的评估次数: ", nrow(model_performance)/length(unique(model_performance$Model)), "\n"))
  cat(paste("  • 总评估样本数: ", nrow(model_performance), "\n"))
  
  cat("\n=============================================\n")
})
  
  # 5.27 优化建议输出
  output$optimization_recommendations <- renderUI({
    risk_score <- portfolio_results$risk_score
    risk_level <- portfolio_results$risk_level
    
    if (risk_score == 0) {
      return(tags$div(
        class = "alert alert-info",
        tags$h4("ℹ️ 请先计算投资组合风险"),
        tags$p("请先计算投资组合风险以获取优化建议")
      ))
    }
    
    # 根据风险等级提供建议
    if (risk_level == "低风险") {
      tags$div(
        class = "alert alert-success",
        tags$h4("✅ 投资组合风险较低"),
        tags$p(paste("当前投资组合风险评分为", round(risk_score, 1), "，风险可控。")),
        tags$hr(),
        tags$h5("建议操作:"),
        tags$ul(
          tags$li("保持现有配置"),
          tags$li("定期监控市场变化"),
          tags$li("可考虑增加低风险稳定币以进一步分散风险")
        )
      )
    } else if (risk_level == "中风险") {
      tags$div(
        class = "alert alert-warning",
        tags$h4("⚠️ 投资组合风险中等"),
        tags$p(paste("当前投资组合风险评分为", round(risk_score, 1), "，存在一定风险。")),
        tags$hr(),
        tags$h5("建议操作:"),
        tags$ul(
          tags$li("减少高风险稳定币的配置比例"),
          tags$li("增加USDC等低风险稳定币的配置"),
          tags$li("设置止损点"),
          tags$li("密切关注高风险资产的市场动态")
        )
      )
    } else {
      tags$div(
        class = "alert alert-danger",
        tags$h4("🚨 投资组合风险较高"),
        tags$p(paste("当前投资组合风险评分为", round(risk_score, 1), "，风险较高。")),
        tags$hr(),
        tags$h5("紧急操作建议:"),
        tags$ul(
          tags$li("立即减少高风险稳定币持仓至30%以下"),
          tags$li("优先增加USDC配置"),
          tags$li("设置严格的止损机制"),
          tags$li("每日监控投资组合风险"),
          tags$li("考虑使用对冲工具降低风险敞口")
        )
      )
    }
  })
}

# 6. 运行应用程序 ------------------------------------------------------------
shinyApp(ui = ui, server = server)
