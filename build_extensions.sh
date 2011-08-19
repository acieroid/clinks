ZIP=$(which zip)
MV=$(which mv)
CHROMIUM=$(which chromium)
FIREFOX_EXTENSION_DIR=firefox/
CHROME_EXTENSION_DIR=chrome/
FIREFOX_EXTENSION_DEST=clinks.xpi
CHROME_EXTENSION_DEST=clinks.crx
CHROME_CERT_DEST=clinks.pem

build_firefox_extension() {
  which zip > /dev/null
  if [ $? -eq 1 ]
  then
    echo "NOT building firefox extension (zip is not installed)"
  else
    echo "Building firefox extension ($FIREFOX_EXTENSION_DEST)"
    cd $FIREFOX_EXTENSION_DIR
    $ZIP -r clinks * >/dev/null
    $MV clinks.zip ../$FIREFOX_EXTENSION_DEST >/dev/null
    cd - >/dev/null
  fi
}

build_chrome_extension() {
  which chromium > /dev/null
  if [ $? -eq 1 ]
  then
    echo "NOT building chrome extension (chromium is not installed)"
  else
    echo "Building chrome extension ($CHROME_EXTENSION_DEST and $CHROME_CERT_DEST)"
    $CHROMIUM --pack-extension=chrome/ --no-message-box
    $MV chrome.crx $CHROME_EXTENSION_DEST
    $MV chrome.pem $CHROME_CERT_DEST
  fi
}

build_firefox_extension
build_chrome_extension
