# ======================================================
# Anchor Watch 一键发布 GitHub 脚本
# 请在 Positron 的 R 控制台（Console 标签）中运行
# ======================================================

# 1. 项目根目录路径（已改为你的真实路径）
project_dir <- "D:/A大学课程文件/26 智能财务机器人/小组作业_稳不稳"

# 2. 你的 GitHub 用户名
github_user <- "Writteninwater713"

# 3. 你在 GitHub 上新建的仓库名（随便取，不要有空格）
repo_name <- "anchor-watch"

# 4. 分支名
branch <- "main"

# ======================================================
# 以下代码无需修改，直接运行
# ======================================================

# 检查 Git 是否安装
if (system("git --version", intern = FALSE) != 0) {
  stop("❌ 未安装 Git，请先安装：https://git-scm.com/downloads")
}
cat("✅ Git 已安装\n")

# 检查项目目录是否存在
if (!dir.exists(project_dir)) {
  cat("❌ 路径不存在：", project_dir, "\n")
  cat("   请确认项目文件夹位置，或在 R 控制台运行 getwd() 查看当前工作目录\n")
  cat("   如果工作目录就是项目目录，可以把 project_dir 改为 getwd()\n")
  stop("路径错误")
}
setwd(project_dir)
cat("📁 当前目录：", getwd(), "\n")

# 创建 .gitignore（如果已有会自动覆盖，内容安全）
writeLines(c(
  "# R 临时文件",
  ".Rproj.user",
  ".Rhistory",
  ".RData",
  ".Ruserdata",
  "*.Rproj",
  "",
  "# 系统文件",
  ".DS_Store",
  "Thumbs.db"
), ".gitignore")
cat("✅ .gitignore 已创建\n")

# 初始化 Git 仓库
if (!dir.exists(".git")) {
  system("git init")
  cat("✅ Git 仓库已初始化\n")
} else {
  cat("⚠️ Git 仓库已存在，跳过初始化\n")
}

# 检查并安装 Git LFS
check_lfs <- function() {
  lfs_check <- system("git lfs version", intern = TRUE, ignore.stderr = TRUE)
  if (length(lfs_check) == 0) {
    cat("❌ Git LFS 未安装\n")
    cat("请先安装：https://git-lfs.com\n")
    cat("安装完成后重新运行本脚本。\n")
    stop("必须安装 Git LFS")
  }
  system("git lfs install")
  cat("✅ Git LFS 已就绪\n")
}
check_lfs()

# 设置 LFS 追踪大文件
for (pat in c("*.rds", "*.csv", "*.xlsx")) {
  system(sprintf('git lfs track "%s"', pat))
}
cat("✅ 已设置 LFS 追踪：*.rds, *.csv, *.xlsx\n")

# 暂存所有文件
system("git add .gitattributes")
system("git add .")
cat("✅ 文件已暂存\n")

# 提交
system('git commit -m "Initial commit: Anchor Watch Shiny app"', ignore.stderr = TRUE)
cat("✅ 提交完成\n")

# 设置远程仓库
remote_url <- sprintf("https://github.com/%s/%s.git", github_user, repo_name)
if (!any(grepl("origin", system("git remote -v", intern = TRUE)))) {
  system(sprintf("git remote add origin %s", remote_url))
  cat("✅ 远程仓库 origin 已添加\n")
} else {
  system(sprintf("git remote set-url origin %s", remote_url))
  cat("✅ 远程仓库 origin 已更新\n")
}

# 推送到 GitHub
cat("🚀 正在推送... 如果弹出认证框，请输入 GitHub 用户名和 Token\n")
push_result <- system(sprintf("git push -u %s %s", remote_url, branch))
if (push_result == 0) {
  cat(sprintf("🎉 发布成功！\n\t仓库地址：https://github.com/%s/%s\n", github_user, repo_name))
} else {
  cat("⚠️ 推送失败，请检查：\n")
  cat("  - 是否已在 GitHub 上创建了仓库：", repo_name, "\n")
  cat("  - 用户名或 Token 是否正确\n")
  cat("  - 如果遇到 LFS 带宽限制，可以暂时移除大文件（将 .rds / .csv 加入 .gitignore）\n")
}