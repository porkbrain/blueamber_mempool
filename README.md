# blueamber_mempool

## Background

Distributed network Blue Amber (BA) is a network on which agents work together towards some goal. It consists of two types of agents: **mempools** and [client nodes][todo-client-repo]. BA was designed to give all agents a way to contribute to a common goal without having to trust other agents on the network. We refer to the goal as to **task**.

The BA network is not concerned about what are the client nodes working on. However, there are some characteristics which the **task** and its **solutions** should have in order to be a fit for TODO network.

1. Message size TODO
2. Message should be everythng one needs to progress TODO
3. An algorithm that solves the task should output intermediate results. We refer to these results as to **solutions**. The solutions are carried by messages to mempools where they are picked by other nodes. Hence the algorithm has to implement a way of enhancing itself when combined with solutions of other nodes.
TODO. Solutions size should be small so that thousands of them can fit into memory and they can be TODO. Each task has an associated max solution size constant that can be used to calculate how many messages can mempool fit. Another reason for small sizes is network traffic.
4. An algorithm should have a way to deal with noise - malicious messages with purpose of detune the network. Evolution algorithms do this implicitly. Other algorithms might need some kind of defensive evaluation to determine whether the message is genuine or noise, such as similarity threshold. Similarity threshold can quickly determine if the message carries a solution which gives output that is at least remotely similar to what have a client node seen before. Note that in some cases the noise is desirable to an extend and serves as random mutation.
5. If the task at question should be worthwhile computing, the messages must adhere to some kind of protocol. This suggests that while each client node can run different algorithm, they should have the same output format.


TODO: Examples of algorithms that can be used

## Mempool

Mempool is an agent in BA whose purpose is to store messages from client nodes and make them world accessible. The storage is temporary (although depending on the mempool load a message might survive for a long time).

### Request lifetime

Example of `GET` request which returns _N_ messages.
```
|   a request PID   |   the memtissue PID    |      a memcell PID     |
|-------------------|------------------------|------------------------|
->
    TCP
    validation
    get N msgs      ->
    .                   random cell
    .               <-  cb to get N msg
    calls cb        ------------------------->
    .                                           gets N random msgs*
    .               <-------------------------  puts them into list
    repeats (?)
<-  formats
|-------------------+------------------------+------------------------|
```

_(* If cell does not contain enough messages, the number of messages can be less then N. In this scenario the default behavior is to repeat until enough messages or threshold of calls is reached.)_

## Build

    $ rebar3 compile

<!-- Invisible List of References -->
[todo-client-repo]: TODO
