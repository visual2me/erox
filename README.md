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
* ./rel/erox/bin/erox start
* connect Open vSwitch to erox controller
  ```sh
  $ ovs-vsctl set-controller br0 tcp:[erox_ip_address]
  ```
