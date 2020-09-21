title: Installing debian mips on qemu
slug: debian-mips-qemu
category: computing
date: 2019-05-09
modified: 2019-05-18
<!-- Status: draft -->


Hi, recently I had to install a little endian machine to test out some old C code for academic ressons. I had choices of using Sun solaris for SPARC workstations or using a mips machine. I went on to install debian wheezy on qemu mips.

First download the appropriate vmlinux kernel and harddisk qcow2 image from Aurélien Jarno's ftp site.
[https://people.debian.org/TLIDEaurel32/qemu/mips/](https://people.debian.org/~aurel32/qemu/mips/)

```bash
./qemu/build/mips-softmmu/qemu-system-mips \
-m 256 \
-M malta \
-kernel vmlinux-3.2.0-4-4kc-malta \
-hda debian_wheezy_mips_standard.qcow2 \
-append "root=/dev/sda1 console=tty0" \
-net user,hostfwd=tcp::10022-:22 \
-net nic
```

```bash
sysctl -w net.ipv4.ping_group_range='0 2147483647'
```
Now you can ssh into qemu. Password is root 
ssh root@localhost -p 10022  
