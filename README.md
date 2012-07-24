erox: Erlang OpenFlow Controller
====

erox is an Erlang OpenFlow Controller, which aims to be an tool for OpenFlow-enabled switches management and configuration.

Building
--------

### Dependencies
- rebar (<https://github.com/basho/rebar/>)

### Building erox
```sh
$ git clone https://github.com/visual2me/erox.git
$ cd erox
$ make compile
$ make rel
```
### Using erox
* ./rel/erox/bin/erox [start | console]
* connect Open vSwitch to erox controller
  ```sh
  $ ovs-vsctl set-controller br0 tcp:[erox_ip_address]
  ```
* show connected switches in erox
  ```sh
  Eshell V5.9.1 (abort with ^G)
  1> switch_man:show().
  ```
* disconnect Open vSwitch from erox controller
  ```sh
  $ ovs-vsctl del-controller br0
  ```
