## Peer to Peer Networking in Tezos


### P2P Networking

A centralized network is organized as a server, with a bunch of clients that
connect to it to access some service. The two main issues with this model
are that it is not very resilient and it is not very scalable. If the server
goes down, then the whole service becomes unavailable. It is not scalable because one server has to serve all the clients.
When the clients grow in number, the server also has to grow in capacity, to meet
the increased demand.

P2P networks are a solution to this problem. In a P2P network, every
client is also a server. We call this client+server combination a 'node' or a 'peer'.
This adds resilience and scalability to the network.

If one node goes down, it
does not disrupt the service as any number of other nodes can act as a server.

It is also scalable because each client added to the network is also a server
added. So as the number of clients in the network grows, the number of servers
also grows automatically with it.

A perfect example of this is the BitTorrent network. You can
download a torrent many times faster than any of the contributing nodes can
upload because your download is sourced from multiple nodes. All their
bandwidth, though moderate when taken in isolation, adds up to the amazing
download speed that you observe on your end. If one peer disappears, your
downloads might slow down a bit instead of halting completely.
Similarly, when you join the network, every other peer will notice that their
downloads are slightly accelerated as your node starts contributing to them.

But one can ask how a node can act both as a server and as a client? If every
node can act as a server, why don't they serve themselves? This is because any single node
often does not have all the required data available to serve every
possible request. It can only act as a server to those requests that ask for
the data that is in its possession.

Now that we have covered the basics of P2P networking, we will look at how
P2P is relevant to cryptocurrencies.

### P2P networking and blockchain

A blockchain is a distributed ledger. A ledger, as you might already know,
records transactions. Since the ledger is distributed, every asset holder in the system
will have a copy of the ledger. Since everyone would like to have the most updated copy of
the ledger, all the participants in a distributed ledger system need to be in
constant communication. In addition, we need the network to be always working.
If the network stops working, your money stops working.  And any kind of
money that could randomly stop working is not a very good kind of money. So we
need this network to be resilient. And we need the network to be scalable.
Because as more and more people/accounts get added to the ledger, the more
processing and resources it takes to process and validate the growing number of
transactions. For exactly these reasons, blockchains use P2P
networks in their implementations.

### Tezos P2P network

Every peer in a Tezos network is actually comprised of [a bunch of processes](https://tezos.gitlab.io/introduction/howtorun.html#baker).
At the center of all these processes is the actual `tezos-node` process. Then we
have the baker, endorser, accuser processes that interact with the node process
via RPC to inject various operations into the network. But to do that, they
need to know what is going on in the network, and thus, as we saw earlier,
the nodes constantly need to talk to other peers to know what is going on out there.
This means that the servers can get overwhelmed pretty fast if new nodes joining the
network are not capable of sharing some of the load.

So ideally, like in any P2P network, we would want every node added to the
network to act both as a server and a client. But some aspect of our current
Internet technology makes it a little difficult for prospective nodes to
act as a server. Enter NAT.



### Network address translation (NAT) and P2P networks

We shall first look at how NAT works. But to do that, we should first understand
the problem that NAT is supposed to solve.

NAT is employed when you want to share a single network address among multiple
network entities. Normally this is employed in IPv4 networks (which currently cover most of today's Internet), and owing to the fact that there are only a limited number of IPv4 addresses, it makes some sense to share
one address among many hosts.

To understand how this works and its limitations, let us first
look at how a TCP connection is established. When a computer
wants to initiate a connection to another computer on an IP network, it sends a special IP packet called an `SYN` packet (which is just a bunch of data with certain bits set) to the server.

Upon accepting the connection, the server sends another type of IP packet called `SYN-ACK` back to the client.
The client, upon receiving this, sends an `ACK` packet back to the server, completing the handshake
and establishing the connection.

When we say "establishing the connection", it just means that in the network
layers of both client and server, an entry is added that records the fact that
a connection handshake was successful, along with a bunch of bookkeeping data.

Once the connection is established, any further packets coming from the other
end, identified by a remote IP/Port pair and a target IP/Port pair, will be
associated with this connection.

As to why this "connection" is required, it is because IP is a stateless
protocol and the IP packets may arrive out of order, or some might even go
missing. So the IP layer gets the data across, and TCP layer ensures that it
is delivered to the application without errors and in the correct order, and
the data or state associated with the connection is what the TCP implementation
uses to recover from such issues.

Anyway, the important thing which is key to understanding how NAT works is that when a client with address `A` initiates a connection from TCP port `Ap` to client `B` at TCP port `Bp`, all the packets that form the connection, including the initial handshake (`SYN`,`SYN-ACK`,`ACK`), will have
`A`, `Ap` and `B`, `Bp` as source/destination address/port pairs (Or the other way around if it is a reply packet). This enables the NAT router to implement specialized routing just for the packets that belong to this connection.

Though we looked at a TCP connection here, NAT does not require a 'connection' and
can also work with connectionless protocols like UDP. We looked at TCP just because
it is more relevant in the context of Tezos P2P networks.


#### NAT by example

Let us look at an example. Say you have a bunch of computers connected in a LAN, and one of them is
connected to the Internet. Imagine you have statically assigned IP addresses
for each of these machines. We call these "private addresses". This is because
these addresses do not have a meaning outside the context of your LAN, just like how a hotel room number is meaningless if you don't specify which hotel. So
suppose that all the machines in your LAN have private addresses, and the machine that
is connected to the Internet has a public IP address in addition to the
private IP address from the LAN network.

So, to make Internet available to all the computers in your LAN, you configure
the computer that is connected to the Internet to be a NAT router.

Let us see what happens when one of the machines, say, one with the private IP
address of `192.168.0.10`, wants to start an HTTP connection to `freenode.net`
to HTTP port 80. It tries to establish a connection by sending a `SYN` packet to
`freenode.net` to port `80`. This packet reaches the router (let its private
address be `192.168.0.1`), and on seeing this `SYN` packet to `freenode.net`, which marks the beginning of a TCP
connection, it replaces the source address, that is `192.168.0.10:5000` with its own address
`123.43.22.10:13000`, and sends it out to the Internet. Along with that, it
makes a note that a connection initialization packet that came from
`192.168.0.10:5000` and it was sent out to the Internet with its own source address `123.43.22.10` and port `13000` (Regarding how the router selects the port `13000` here, there could be some logic to it, but let us not concern ourselves with it just yet, and let us imagine that the router picks a free, unused port arbitrarily). When the reply
`SYN-ACK` packet comes back from `freenode.net:80`, it will be to port `13000`,
and then the NAT implementation (after looking up the table) knows that it is meant for port `5000` of host
`192.168.0.10` in the NATed private network. So, it forwards the packet to that host. Before that, it also changes the destination address to `192.168.0.10` and destination port to `5000`. It leaves the source address as it is. So, when it reaches the host, it would be as if the packet came directly from `freenode.net`.







In this way, by intercepting outgoing `SYN` packets and maintaining a
record of how it has mapped them to its own IP/Port pairs, it can support a two
way data flow required to maintain a TCP connection, providing the
appearance of an Internet connection to the hosts that it serves.

The important thing to note here is that this only works because a host behind
the NAT initiated a connection to the remote IP address, via the `SYN` packet,
enabling the NAT router to intercept the outgoing connection. So without
explicit configuration, there is no way a computer on the Internet, can
initialize a connection to a host behind the NAT.

This means that if a node behind a NAT wants to join a P2P network, its role in the network will be limited. We saw earlier that ideally, when a node joins the network, the network should gain a client as well as a server. But when a node behind a NAT joins the network, it only gains a client. But still, it can serve the reachable nodes in the network through connections initiated by itself. (We have seen that if you have two nodes where there is only one-way connectivity, blocks can flow in the other direction as well.)

### Peer discovery

Peer discovery is the process of discovering other peers in a network. BitTorrent protocol use tracker servers to maintain a list of peers in a swarm. So, when you start to download a file using a torrent, the client can immediately get a list of peers from the tracker information in the torrent file.


Similarly, a Tezos node also needs to know where to start its connection to the P2P network. When you start a Tezos node, you can pass an explicit list of peers to
the node via the `--peer` argument. For example, you can use the following foundation nodes to bootstrap your node:

    dubnodes.tzbeta.net
    franodes.tzbeta.net
    sinnodes.tzbeta.net
    nrtnodes.tzbeta.net
    pdxnodes.tzbeta.net

But if this argument was not provided, the [node will try to connect to `boot.tzbeta.net`](https://gitlab.com/tezos/tezos/-/blob/72089626c46193dfd5e9e6be6683d7b49dcd87b6/src/bin_node/node_config_file.ml#L103). This is a domain name that has many nodes behind a load balancer.

Tezos node also has another technique to discover local peers, that is, peers in the local network. You can use the `--discovery-addr` option
to provide the broadcast address of a network, and the node will send periodic UDP broadcast packets that contain information like peer id and the listening port. Any node that receives this can try to connect using the contained information and the source IP address in the UDP packets.

### Accepting incoming connections

When the node runs with a publicly available IP address, directly on the host
(that is not in a container or a virtual machine), accepting incoming connections
is trivial.

But for most users that want to run a node on their home computer, that is not the case. Let's look at specific cases.

#### 1. They are behind a NAT

Most home Internet users don't have a public IP address because they often
access the Internet via an ISP. ISPs tend to run a Carrier-grade NAT so that they can
share a public address among many users. On top of that, if the user has
a router at home, with their Internet connection shared between multiple
devices, there would be another layer of NAT.

Let us see what happens with just one layer of NAT between
a user's home computer and the Internet. Let the internal IP address of the user's
computer be `192.168.0.10`, their router's internal IP be `192.168.0.1` and its
public IP be `20.20.20.20`.

When the node starts up, it will start to listen on `192.168.0.10` at port `9732`.
Suppose it also tries to connect to a hard-coded peer address at `boot.tzbeta.net` at
port `9732`. So this connection starts as a connection from `192.168.0.10:5000`
to `boot.tzbeta.net:9732`. As it passes through the router, it becomes a connection
from `20.20.20.20:6000` (where port 6000 is selected by the router) to `boot.tzbeta.net:9732`. Because of NAT, this connection
works and the initial connection information is passed on, which (among other
details) contains the port at which the node is listening, which is `9732`. Thus
at `boot.tzbeta.net`, a new peer with "point" (which is just a name for a IP/Port pair), `20.20.20.20:9732` gets added to its
peer list. All is fine until now, but imagine what happens when a third node
joins the network via `boot.tzbeta.net`. When this third node fetches the list
of peers from the boot node, the point `20.20.20.20:9732` also gets passed along.
But if this new node tries to connect to `20.20.20.20` at port `9732`, it fails
because there is no server running at the router. And the router does not forward
the connection to the actual host because NAT only comes into play when the
connection was initialized from the internal host. So here,
even though our user's node was able to connect to a peer, it failed to fulfill its
role as a proper peer in the network by not being able to accept an incoming connection.

#### 2. They run node in a container on Windows/macOS

Tezos applications are often distributed in a containerized form as
a Docker app. This makes it easy for people running different operating systems
to participate in the Tezos ecosystem. But this also adds to the complexity of
allowing incoming connections to the node.

Docker provides multiple ways to make the host's network available to programs
running inside containers. One such option is called 'host networking',
done via the argument `--network=host`. It makes the entire network of the host available inside the container. From a networking perspective, this is as
if the program was running on the host machine itself.  But this option
is only available if you are running Docker on a Linux host. That is, if you
are on Windows or on MacOS, this option is not available. We will see the reason for this in a bit.

Another option provided by Docker is called port forwarding. While using this option, the container's network is isolated from the host's network, but it forwards the network communications that arrive on a specific host's ports to specific ports in the container. This is done via
the `-p` argument, which takes the port of the host that should be forwarded, and
the port of the container to which it should be forwarded to.

So, an HTTP server running inside the container can be made to serve external clients by forwarding the port `80` of the
host network to port `80` inside the container.  Now, any network
communication that reaches port `80` in the host will be forwarded to the port `80`
inside the container.

On Linux, this port forwarding is done via NAT routing. We have seen earlier that in NAT routing, the source address in the incoming connection is preserved. Docker adds a network
interface on the host to do this routing. To simplify, if you remember the
description of NAT from the earlier section, this can be considered as an always on
NAT, without the need for the host behind the NAT to initiate the connection.
If the port `9732` of the host is forwarded to port `9732` in the container, then any
network connection that is coming to the host's port `9732` will be unconditionally
forwarded to the containers port `9732`. So, in this configuration, the node behind the NAT will see the connection come from the external peer itself.

But again, on Windows and macOS, it is a different story.

##### Docker on Windows/macOS

Running a container requires a set of tools/APIs that are only available natively
in Linux. When you run a Docker container on Linux, the Docker program uses these
native APIs to launch the container. But when your run Docker on Windows or MacOS,
these APIs are not natively available, and Docker has to first launch a Linux virtual machine and then use the APIs that are available in the Linux machine to launch the container inside it.

This means that the Docker's network interface is added inside the virtual
machine and this makes it hard to route the packets to the Docker
container the same way it is done in Linux.

So instead, it is done via a TCP proxy. A TCP proxy is different from
NAT routing in the sense that the proxy creates an entirely new
connection to the target and just copies the TCP payload between the original
and forwarded connection.

As a side effect of this, the server receiving the connection is completely
robbed of the source information of the original connection, and just sees
the Docker's internal IP as the source address in the incoming connections.

And since the node uses the source address in a connection to assign points (which are just IP/Listening port pairs),
any new connection that comes in from a peer who listens on the same address as an existing peer will be rejected
as a duplicate (Because from the perspective of the node, they both share the same source address and listening port). And thus, even though your node could have served as a fully
capable public node, the Docker's workaround on Windows/macOS limits that to
the ability to serve a single remote connection at a time.



#### 3. Running Tezos using Kubernetes in Linux hosts

In addition to the issues in Windows/macOS, if you are running the Tezos node on Linux, using the Kubernetes, for example [this repo](https://github.com/tqtezos/tezos-k8s),
you should follow [some additional configuration](https://kubernetes.io/docs/tutorials/services/source-ip/) to make source addresses available in the node.


### Some common scenarios when running a Tezos node

#### If you have a public IP address and you are not running the node in a container

You can run a publicly reachable node here. You have a public IP address, and since you are not in a container, you can make your node listen on your public IP itself. So, the node can receive incoming connections to that public IP.


#### If you have a public IP address, but your node is running behind NAT

You can run a publicly reachable node here if you can get your NAT router to forward ports
to your node. Most home wifi routers will have an admin interface accessible
at some predefined local address. Once you are in that interface, you should be able
to access some kind of configuration sub page to set up port forwarding, among other
things. Note that there could be another layer of NAT (called [Carrier-grade NAT](https://en.wikipedia.org/wiki/Carrier-grade_NAT)) at your ISP, and if that is the case, the port forwarding configuration on your home router will not help.

#### If you have a public IP address, but want multiple nodes to run behind NAT

Since any remote node will see any connection coming from any of your NATed nodes
as coming from the same source address, only the first node that connects to
the remote node can get connected. If other nodes are listening on the same port and if they try to connect to the
same peer, they will be rejected with the 'peer already connected' message. This happens because, from the perspective of the remote peer, both connections come from the same 'point' (source IP address/listening
port pair). Note that the source port of the incoming connection is not relevant. Only the listening port, which is a part of the payload, is used to pair with the source address to form the point.


#### If you have a public IP address and you are running the node in a Docker container

If you are on Linux, this is mostly the same as running the node on the host itself. So, you can accept incoming connections to the node.

But if you are on Windows/macOS and run the node in a Docker container, even though your node will be able to
accept incoming connections, you will be limited to accepting a
single connection at a time.  This happens because the container can only see
the forwarded connection from the Linux virtual machine. And since all of these
have the source address set to Docker's internal router's IP address, the Tezos
node considers all the connections coming from the same point and rejects all
but one among them.

#### If you have a private IP address

You cannot accept incoming connections without additional configuration.
You can still participate in the P2P network by connecting out to the list of
peers fetched from bootstrapping nodes. When you run it this way, you will
find that your node has a lot fewer connections than a node that is able
to accept incoming connections. If you want more connections, you can make connections
manually (or via a script) by querying the IP address at `boot.tzbeta.net` or
at foundation node server names, and connecting your node to those peers via `tezos-admin-client`, which should be available from the Tezos package itself.

You might be able to get a public IP address for your node by either of the following ways:

1. Contact your ISP and get a public IP address. It would cost you more than having a NATed IP, though.
2. Host the node in a server with a public IP address, like an AWS instance or a DigitalOcean droplet.
3. Use something like [ngrok](https://ngrok.com/) as described [here](https://tezos.stackexchange.com/questions/2659/how-to-publish-the-public-ip-address-of-my-node-to-the-network) to
create a TCP tunnel to a public address to your local node.

In the third solution, we come across a shortcoming of the Tezos node CLI
interface. As of now, it does not allow a node to listen on an address and publish a
different address to the network for other peers to connect to it. The
situation in 3 can be summarized as a case where a user runs a node behind a NAT on a
local port, but has created a tunnel with a public IP address to the local port. But there is no way for
the user to make the node publish this public address when it connects to
a remote peer. And, as we saw earlier, the remote peer only has the option of
reading the source IP from the incoming connection.

If you want to publish this public address, you will have to use the
`tezos-admin-client` program to tell a public node on the network to connect
to the public address of your node. This causes the public address of your node
to be added to the peer list of the remote public node and thus get published to the
network.

[This](https://gitlab.com/tezos/tezos/-/issues/260) issue in Tezos repo reports this
problem and [this comment](https://gitlab.com/tezos/tezos/-/issues/260#note_370996071) describes why the
lack of this option is not optimal. We hope something will be done about it soon.

### Network health of a P2P network

Network health of a P2P network can be loosely defined as a measure of
connectivity in the network. A network that can be disabled by taking out a
small subset of peers is not very resilient, and thus of poor health. A network
where it is hard to find a small subset of nodes that can be taken down to take
the whole network down can be thought of as more resilient, and thus having a
better network health.

It is not required that all peers in the
network have the capability to receive incoming connections, as long as there
are some peers that are reachable. This is because a P2P connection is bidirectional,
and can carry similar information irrespective of the node that initiated the connection.
So if the number of unreachable peers that join the network is more or less similar to
the number of public nodes that join it (or even a small multiple of it), then the network health
remains good, because these peers could serve each other. But when the number of unreachable
peers become an overwhelming majority of the nodes joining the network, then we could have a
problem, because this means that the operation of the network is dependent on those few public
nodes.

This is not much of an issue in some P2P networks, but in something like Tezos, we
would want a network that has very little chance to go down, even under persistent
attacks. This means that we would want as many publicly reachable peers as possible. So it is quite
important that the community provide tools that enable its members to easily
run such nodes without incurring high costs for themselves.

#### NAT Traversal techniques.

NAT traversal is the process of establishing a connection to a NATed host.
This is a complex topic because there are different types of NAT, with some kinds
of NAT being easy to traverse, while other not so much so.

Generally, NAT traversal
techniques can be divided in three types. Let us look at them.

##### 1. Hole punching

This involves the peer doing something, usually sending a packet out through the NAT router
that adds a connecting tracking entry at the NAT router. This entry essentially
acts as a "hole" in the NAT, through which an incoming connection can pass through and
connect with the host behind the NAT.

##### 2. Reverse connection

This technique requires a third server that is not behind a NAT. Also, this works only
when only one of the two peers that want to be connected is behind a NAT. Essentially
the peer that is not behind a NAT relays a message to the peer behind the NAT via the third
public server to open a connection to itself. Upon receiving this message, the peer behind
the NAT opens a connection to the public peer. This works because the connection is initiated
from within the NAT and thus has no trouble reaching the other peer that is reachable
at a public address.

##### 3. Relaying

This technique is required to traverse the strictest kinds of NAT out there. It essentially
employs a third public server to relay messages between the peers. This can work even if both
peers are behind NAT.

Note that in all the above methods, services of a third, publicly accessible server are required. In the case of hole punching, and reverse connection, the third server's role can be minimal. But in relaying, the third server has to use considerable resources to relay traffic between the communicating peers.

#### Types of NATs

Now, we can look at different types of NAT and how these techniques can be applied to traverse
each of them.

##### 1. Full-cone NAT

We saw earlier that NAT works by tracking outgoing TCP packets and mapping
their source address and port to the routers own port. We have also seen how an
incoming packet to the mapped port in the router is forwarded to the respective
port in the internal host.

In full-cone NAT, the router does this forwarding unconditionally. When a
packet arrives in the router's mapped port, it just forwards the pack to the
internal hosts IP/Port pair from its mapping table, without bothering to check
if the incoming packet actually came from the original destination of the packet
that added the tracking entry in its connection tracking table.

##### 2. Restricted-cone or Address-restricted-cone NAT

Here, the NAT router only lets an incoming packet through if it is coming from a
source address that was the destination address of the packet that added
the connection tracking entry.

##### 3. Port-restricted-cone NAT

Here the NAT router only lets an incoming packet through if it is coming from a
source address and source port that was the destination address and destination port
of the packet that added the connection tracking entry.

##### 4. Symmetric NAT

In all the above three types of NAT, every packet coming from the same host/port
pair is sent out through the same router port, irrespective of where the packet is going.

But in symmetric NAT, a new port is used whenever the destination of the packet is changed.
So imagine a case where a host sends two packets, one to `freenode.net` and other to
`freenode.info`. Imagine the host used the same port to send these two packets.

In full-cone, address-restricted, and port-restricted NATs, both of these packets
will go out through the same port in the router. But in symmetric NAT, a new port
will be used to send the second packet out because it is to a different destination.

These NAT types are officially described in [rfc3489](https://tools.ietf.org/html/rfc3489) and the above are (hopefully) simplified descriptions of them.

You can check the kind of NAT you are behind by using the `stun-client` program
and the public STUN server at `stun.stunprotocol.org` with it via the command
`stun stun.stunprotocol.org`.

For example, this is a sample output:

```
STUN client version 0.97
Primary: Independent Mapping, Port Dependent Filter, random port, will hairpin
Return value is 0x000006
```

You can figure out the kind of NAT that you are in from the output [as follows](http://wiki.snom.com/Networking/NAT):

```
Independent Mapping, Independent Filter = Full-cone NAT
Independent Mapping, Address Dependent Filter = Restricted cone NAT
Independent Mapping, Port Dependent Filter = Port-restricted cone NAT
Dependent Mapping = Symmetric NAT
```

We will quickly look at some NAT traversal protocols now.

#### STUN - Session Traversal Utilities for NAT

STUN is the simplest NAT traversal protocol. It can be used to traverse full-cone, restricted and port-restricted NATs.

The STUN protocol involves a public server on the Internet that runs it.
This server allows hosts on the Internet to send a query to it,
asking for the public IP/Port that the server sees in that request. And this
is exactly the "hole" in the client's NAT through which the packet was sent
out to the STUN server. And it is the same "hole" through which a connection
could be sent through the NAT to the host.

In the case of the full-cone NAT, this works because it does not validate the
source. So, any other server on the Internet, (other than the STUN server, to
which the original request was sent) can send a connection through the
"hole" as long as it remains open.

For address/port-restricted NATs, it involves a little more coordination between
the peers. So this is how it works in that case. Suppose `A` and `B` are two NATed
peers who want to form a P2P connection. Initially, both contact the STUN server
to obtain their NAT holes in the form of public IP/port pairs, `(Apub, Aport)` and
`(Bpub, Bport)`. Then, this information is exchanged using a third server. So, A now
knows of `(Bpub, Bport)` and `B` of `(Apub, Aport)`. Now, if both send a packet to the
other peer's public IP and Port, this causes a "hole" to open in their respective
NATs through which packets from the other peer's public "hole" can come through and
establish a connection.

For symmetric NATs, this cannot work at all. Because the "hole" will change, or,
in other words, a different port will be used when the hole punching packet to
the peer is sent. So, it will be different from the one that was obtained
from the STUN server and was shared with the peer.





#### TURN - Traversal Using Relays around NAT

TURN works by involving a third server on the public Internet to act as a relay between
the two peers that want to create a P2P connection. A downside of this approach is that the relaying
server should spend its bandwidth to act as a relay.

#### ICE - Interactive Connectivity Establishment

ICE is a somewhat complex protocol. It is supposed to be most effective and be
able to traverse any kind of NAT. ICE actually is a combination of many NAT
traversal methods that are tried at both ends of a P2P connection, and after
figuring out a set of methods that works at both ends, it proceeds to employ them in
turn until a connection is established.

Some resources that describe various NAT traversal techniques described above can be seen [here](https://slideplayer.com/slide/4171149/) and [here](https://www.shixuen.com/router/nat_traversal.html).

#### Use IPV6

This is not really a NAT traversal technique. But if you have IPV6
connectivity, chances are that your address is public. This is because, unlike
IPv4 addresses, IPv6 address space is huge. It is so big that if we were to
assign an IPv6 address to every atom on Earth's surface, we could assign
thousands of addresses to each one of them. Despite this, IPv6 adoption is
currently going slow. So it might be a while before we will all have our own
public Internet addresses without additional cost. But until that time, there
are services that allow you to tunnel IPv6 traffic through IPv4 networks like [this](https://en.wikipedia.org/wiki/Teredo_tunneling).

### ZeroTier

ZeroTier is a service that allows you to build LANs that can span the entire
globe. Imagine you have two computers separated by a large distance. Installing
ZeroTier on both of these machines enables you to access the other machine
as if it was part of your local LAN. This means that you can get to it without
going through a NAT or a proxy, by using the ZeroTier-assigned, private
network address. To be clear, it goes through NAT, but the ZeroTier service
just makes it transparent. But despite this, you cannot use ZeroTier to put
your node on the public Internet. It just makes it reachable from other ZeroTier
nodes over the Internet.

### NAT Traversal and Tezos

Let us look at how some of these techniques can be applied in the context
of Tezos P2P.

#### Hole punching using STUN

Imagine a node 'A' (that is behind a NAT) connecting to a public node, `P` and fetching
a list of peers, which contains 'Bpub', the public address of node `B` that is
also behind the NAT.

Imagine a case where 'A' wants to form a P2P connection with 'B'. We know STUN
requires some minimal coordination from a third server. We suppose we can ask
'P' to do this coordination.

So 'A' asks 'P' to relay to 'B' a request to punch a hole in its NAT for `Apub`.
'P' can do this because 'B' is connected to 'P' via a connection initiated from 'B'.

Upon receiving this 'P' sends a timing reply to both 'A' and 'B'. The message
to 'B' includes 'Apub', and, on receiving this, 'B' sends a packet to 'Apub',
punching the hole in its NAT. In concrete terms, this involves sending a connection
request to 'Apub' from the same port that 'B' node is listening on.

This packet will be lost at 'A's NAT. But at the
same time, as soon as 'A' receives the reply from 'P', it initiates a
connection to 'Bpub'. Since a hole for 'Apub' is opened at 'B's NAT, 'A's connection
is let through and makes it possible for a connection to be established.

#### Reverse connection

Here one of the nodes has to be on the public Internet. In the example, let us
change 'A' to be in a public address.
In this case, 'A' requests the public peer 'P' to relay the request, and 'P'
just asks 'B' to connect to 'A', which is publicly reachable.

#### Relaying

Here both nodes can be behind a Symmetric NAT, so hole punching using STUN fails
to connect. So 'A' asks 'P' to act as a relay for communications to 'B'. Since
'P' is reachable from both 'A' and 'B', no further negotiations are required.

By using these techniques, it appears that most of the NAT could be traversed.
Services like ZeroTier work exactly by using these techniques. There is no
reason why they could not be implemented in Tezos as well. Admittedly, ZeroTier
also falls back to relaying when everything else fails. But in the [words of the
ZeroTier's founder](https://zerotier.com/the-state-of-nat-traversal/),

    At the end of the day anywhere from 4% (our numbers) to 8% (an older number
    from Google) of all traffic over a peer to peer network must be relayed to
    provide reliable service.

In the case of Tezos, considering that most people who want to run nodes could
be behind NATs, and if only half of them is traversable (ZeroTier estimates
around 95% of them should be traversable), it would still be a big win for
the network health of the Tezos P2P network to make these nodes reachable.

### Conclusion

Tezos P2P layer works pretty well as of now. But it could be better. We have some low hanging
fruits in form of issues such as [this](https://gitlab.com/tezos/tezos/-/issues/260#note_370996071).
Even though many P2P networks work fine without implementing a NAT traversal, it would
be quite nice if Tezos implements some of these techniques and provide supporting infrastructure so that people
can participate in the Tezos ecosystem better, which would in turn result in a robust, responsive and healthy P2P network.
But it is hard to improve things that you cannot measure. So before we try to enhance P2P layer, it
is important to have some kind of measurement of its current state, which is why efforts like
[this](https://baking-bad.org/blog/2020/07/30/inspecting-tezos-network-decentralization-200-public-nodes-1000-in-total/) appear to be a step in the right direction.
