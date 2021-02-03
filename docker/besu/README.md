## How to run Besu

In `runBesu.sh` set the Besu `VERSION` you wish to test and then just run the script in a terminal 

```./runBesu.sh```

When the script is running Prometheus metrics and Grafana will be available at:

`http://localhost:9091` for the list of all available metrics

`http://localhost:3000/login` to access Grafana (login is admin / admin)


### Metrics
Some metrics are already being displayed in Grafana, using part of the dashboard that can be found in `https://grafana.com/grafana/dashboards/10273` and also replicating some metrics being used by the `mantis-ops` grafana dashboard 


### JSON RPC API
JSON-RPC service is available at port 8545
