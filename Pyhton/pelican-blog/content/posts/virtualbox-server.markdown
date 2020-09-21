title: Ubuntu Linux Server on Virtualbox
slug: vbox-srv
category: Computing
tags: programming
date: 2018-05-10
modified: 2019-04-20
summary: hi there.
stylesheets: styles.css
<!--Status: draft-->
## Update 2019-04-20

Just use [vagrant](https://www.vagrantup.com/intro/getting-started/).

=== === === === === === 
 

1. I downloaded the ISO, installed it into virtualbox.

2. In ubuntu server, I installed necessary packages like `apache2, vim` etc using apt.

3. Every virtual machine is by default, connected using [NAT](https://en.wikipedia.org/wiki/Network_address_translation) virtual netwrking. Here the HOST OS manages the address resolution of the Guest VM. We can change the network to bridged adapter, or, set up port forwarding. 

![]({filename}../images/nat_port_forwarding.jpg){  style="display: block; margin-left: auto; margin-right: auto; width: 50%;"}

To go with the latter approach, In the settings window, go to network tabs. Set up 2 ports: `22` for `ssh` and `80` for server.

4. You can now ssh into the vm using `ssh -p 3022 yourname@127.0.0.1`. You can also use scp to copy your files by `scp -r -P 3022 yousite yourname@127.0.0.1:/path/`

5. Access your server in your browser in host OS `127.0.0.1:9980`
<br/>
<br/>
![Vbox]({filename}../images/vbox-serv-final.jpg)


##### See also
1. [Containers](https://en.wikipedia.org/wiki/Operating-system-level_virtualization), [VPS](https://en.wikipedia.org/wiki/Virtual_private_server).
