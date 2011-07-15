ZIP=$(which zip)
MV=$(which mv)
CHROMIUM=$(which chromium)
FIREFOX_EXTENSION_DIR=firefox/
CHROME_EXTENSION_DIR=chrome/
FIREFOX_EXTENSION_DEST=clinks.xpi
CHROME_EXTENSION_DEST=clinks.crx
CHROME_CERT_DEST=clinks.pem

function build_firefox_extension() {
  echo "Building firefox extension ($FIREFOX_EXTENSION_DEST)"
  cd $FIREFOX_EXTENSION_DIR
  $ZIP clinks * >/dev/null
  $MV clinks.zip ../$FIREFOX_EXTENSION_DEST >/dev/null
  cd - >/dev/null
}

function build_chrome_extension() {
  echo "Building chrome extension ($CHROME_EXTENSION_DEST and $CHROME_CERT_DEST)"
  $CHROMIUM --pack-extension=chrome/ --no-message-box
  mv chrome.crx $CHROME_EXTENSION_DEST
  mv chrome.pem $CHROME_CERT_DEST
}

build_firefox_extension
build_chrome_extension