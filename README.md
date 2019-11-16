# Blue Amber

Distributed network Blue Amber (BA) is a network on which agents work together towards some goal. It consists of two types of agents: **mempools** and [clients][todo-client-repo]. BA was designed to give all agents a way to contribute to a common goal without having to trust other agents on the network. We refer to the goal as to **task**.

The BA network is not concerned about what are clients working on. However, there are some characteristics which the **task** and its **solutions** should have in order to be a fit for TODO network.

1. Message size TODO
2. Message should be everythng one needs to progress TODO
3. An algorithm that solves the task should output intermediate results. We refer to these results as to **solutions**. The solutions are carried by messages to mempools where they are picked by other clients. Hence the algorithm has to implement a way of enhancing itself when combined with solutions of other clients.
TODO. Solutions size should be small so that thousands of them can fit into memory and they can be TODO. Each task has an associated max solution size constant that can be used to calculate how many messages can mempool fit. Another reason for small sizes is network traffic.
4. An algorithm should have a way to deal with noise - malicious messages with purpose of detune the network. Evolution algorithms do this implicitly. Other algorithms might need some kind of defensive evaluation to determine whether the message is genuine or noise, such as similarity threshold. Similarity threshold can quickly determine if the message carries a solution which gives output that is at least remotely similar to what have the client seen before. Note that in some cases the noise is desirable to an extend and serves as random mutation.
5. If the task at question should be worthwhile computing, the messages must adhere to some kind of protocol. This suggests that while each client can run different algorithm, they should have the same output format.


TODO: Examples of algorithms that can be used

# Mempool

Mempool is an agent in BA distributed networks whose purpose is to store messages from clients. The storage is temporary (although depending on the mempool load a message might survive for a long time).

## Request lifetime

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

## Installation

1. Install Erlang.

[Ubuntu][install-erlang-linux]

[OS X][install-erlang-mac]

2. Give executable permissions to the .sh files.

`chmod +x build.sh run.sh test.sh`

3. Run `./run.sh` in your terminal. Files are going to be compiled in `ebin` directory.

## Commands

### Build
`./build.sh`

Compiles all files in all subdirectories of `/src` into `/ebin`.
Output directory can't be changed. However you can specify input directory.

`./build.sh directory`

This command compiles .erl files, but it doesn't run them.

### Run
`./run.sh`

Compiles `src` folder and runs `start/0` function in `main` module.

You can change the boot module with `-b` flag.

`./run.sh -b module`

There's also an option to suppress the compilation with `-s` flag.

`./run.sh -s`

### Test
`./test.sh`

Compiles `src` and `test` directory and runs all `_.spec.erl` files.

<!-- Invisible List of References -->
[install-erlang-mac]: http://erlang.org/doc/installation_guide/INSTALL.html#Advanced-configuration-and-build-of-ErlangOTP_Building_OS-X-Darwin
[install-erlang-linux]: https://hostpresto.com/community/tutorials/how-to-install-erlang-on-ubuntu-16-04
[todo-client-repo]: TODO
