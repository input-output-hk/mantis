#!/bin/bash

echo `dirname $0`
cd `dirname $0`

export PW=`pwgen -Bs 10 1`
echo $PW > ./password

rm ./mantisCA.p12

keytool -genkeypair \
  -keystore mantisCA.p12 \
  -storetype PKCS12 \
  -dname "CN=127.0.0.1" \
  -ext "san=ip:127.0.0.1,dns:localhost" \
  -keypass:env PW \
  -storepass:env PW \
  -keyalg RSA \
  -keysize 4096 \
  -validity 9999 \
  -ext KeyUsage:critical="keyCertSign" \
  -ext BasicConstraints:critical="ca:true"
