For using JSON-RPC through HTTPS a certificate has to be created. For this first a password has to be created:
```
export PW=`pwgen -Bs 10 1`
echo $PW > certificate-password.txt
```

Also, a keystore with the certificate needs to be created, which is unlocked with the previously created password:
```
keytool -genkeypair -v \
  -alias mantisCA \
  -dname "CN=127.0.0.1" \
  -keystore mantisCA.jks \
  -keypass:env PW \
  -storepass:env PW \
  -keyalg RSA \
  -keysize 4096 \
  -ext KeyUsage:critical="keyCertSign" \
  -ext BasicConstraints:critical="ca:true" \
  -validity 9999
```

For using the created certificate and password the following configuration needs to be used:
```
mantis.rpc.certificate-keystore-path = "path/to/mantisCA.jks"
mantis.rpc.certificate-password-file = "path/to/certificate-password.txt"
```
