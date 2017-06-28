# etc-client


[![CircleCI](https://circleci.com/gh/input-output-hk/etc-client/tree/master.svg?style=svg)](https://circleci.com/gh/input-output-hk/etc-client/tree/master)

[![Coverage Status](https://coveralls.io/repos/github/input-output-hk/etc-client/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/etc-client?branch=master)


## A Scala based client for Ethereum Classic 

### Prerequisites 

The scala build tool [sbt](http://www.scala-sbt.org/) must be installed. 

A recent [java sdk](http://www.oracle.com/technetwork/java/javase/downloads/index.html#close) must be installed.

### Milestone 3 - transaction execution

**This version of the code supports downloading the blockchain from remote peers and executing the transactions in the blocks. When all transactions up to and including the latest have been executed the client will continue to stay synchronized.**

For the curious ...

In order to build the client checkout the code base and then type

 `sbt dist`

 in the root of the project.

This creates a distribution zip. Unzip that file to create a folder structure starting with

 ```
 etc-client-0.1
 ```

 From there run 
  
 ```
 ./bin/etc-client
 ```

The client attempts to contact the remote peers configured in the configuration file and begin a 
`fast-sync` download. The [configuration file](https://github.com/input-output-hk/etc-client/blob/master/src/main/resources/application.conf)
settings can all be overriden in 

`./config/application.conf`

The client has not been run on Windows.
 
The next milestone involves integrating our client with Mist browser!
  
  

