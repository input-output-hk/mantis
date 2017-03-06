mkdir -p demo/
sbt "; set test in assembly := {}; assembly"
cd demo/

> client-a.conf
echo 'etc-client.network.server-address.port = 9077' >> client-a.conf
echo 'etc-client.network.discovery.bootstrap-nodes = [' >> client-a.conf
echo '    "enode://f50e675a34f471af2438b921914b5f06499c7438f3146f6b8936f1faeb50b8a91d0d0c24fb05a66f05865cd58c24da3e664d0def806172ddd0d4c5bdbf37747e@144.76.238.49:30306", // Parity' >> client-a.conf
echo '    "enode://74fa49d39bb902c23e9bf415535913eb411288fabaa5d46fb16679d0e7d1677814e3fab93e0b7e75f6a9f04498c79f79e9666138a5b85329f2f1984274261dfa@144.76.238.49:30303"  // Geth' >> client-a.conf
# echo '    "enode://7ff0a073bf2ee9e94fae86de914eca0c0266c0b7051f4592ff0e9820706a085919ea667459897cde4efe3f0465f608cf402bde861983ce6ae3f9185df371aea7@138.197.135.19:30303"  // elaineo' >> client-a.conf
# echo '    "enode://6463c5df643b1a0f663ed8ae915aad8fe74e5ce948d357462f0f0c6459dda35d4af92e1e65280c231239e114c35a98d0cfd15b0c5e7e62d8fb94330b59f804c3@127.0.0.1:30303" // local Parity' >> client-a.conf
echo ']' >> client-a.conf
echo 'etc-client.fast-sync.min-peers-to-choose-target-block = 1' >> client-a.conf
echo 'etc-client.db.leveldb.path = "demo/client-a-db/"' >> client-a.conf

> client-b.conf
echo 'etc-client.network.server-bddress.port = 9078' >> client-b.conf
echo 'etc-client.network.discovery.bootstrap-nodes = [' >> client-b.conf
# echo '    "enode://74fa49d39bb902c23e9bf415535913eb411288fabaa5d46fb16679d0e7d1677814e3fab93e0b7e75f6a9f04498c79f79e9666138a5b85329f2f1984274261dfa@144.76.238.49:30303" // Geth' >> client-b.conf
echo '    "enode://f50e675a34f471af2438b921914b5f06499c7438f3146f6b8936f1faeb50b8a91d0d0c24fb05a66f05865cd58c24da3e664d0def806172ddd0d4c5bdbf37747e@144.76.238.49:30306" // Parity' >> client-b.conf
echo ']' >> client-b.conf
echo 'etc-client.fast-sync.min-peers-to-choose-target-block = 1' >> client-b.conf
echo 'etc-client.db.leveldb.path = "demo/client-b-db/"' >> client-b.conf

echo 'Run node connected to two other nodes:'
echo '$ java -jar -Dconfig.file="demo/client-a.conf" target/scala-2.12/etc-client-assembly-0.1.jar'
echo ''
echo 'Run node connected to one other node:'
echo '$ java -jar -Dconfig.file="demo/client-b.conf" target/scala-2.12/etc-client-assembly-0.1.jar'
