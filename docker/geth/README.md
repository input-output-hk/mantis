## How to run Geth

Just run the script in a terminal

``` ./runGeth.sh```

When the script is running Prometheus metrics and Grafana will be available at:

`http://localhost:6060/debug/metrics/prometheus` for the list of all available metrics

`http://localhost:3000/login` to access Grafana (login is admin / admin)


### Metrics
Some metrics are already being displayed in Grafana, using Dashboard from:
```
https://gist.githubusercontent.com/karalabe/e7ca79abdec54755ceae09c08bd090cd/raw/3a400ab90f9402f2233280afd086cb9d6aac2111/dashboard.json
```


### JSON RPC API
JSON-RPC service is available at port 8545




