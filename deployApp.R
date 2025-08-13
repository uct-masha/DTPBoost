appName = "DTPBoost"
account = '<My Account>'

rsconnect::setAccountInfo(name = account, token = '<My Token>', secret = '<My Secret>')

rsconnect::deployApp(appDir = ".", appName = appName,
                     forceUpdate = T, account = account)