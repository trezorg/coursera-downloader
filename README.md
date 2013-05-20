coursera-scala-dl
==================

Scala script that helps to download Coursera lectures files. Just another :)

Installation
------------

    $ git clone https://github.com/trezorg/coursera-scala-dl.git
    $ cd coursera-scala-dl
    $ sbt assembly

Usage
-------

It is possible to set your login and password in a configuration file.
The configuration file can be set by the script parameter *--filename*.
It is also possible just to put the file into the user's home directory or into
a current directory. In this case the file should have the name coursera.conf.

File structure is pretty simple:

    username = some_username
    password = some_password
    className = datasci-001

You can also set those parameters and other from the command line.
Check for details

    java -jar /path/to/cousera.jar --help

or

    $sbt
    >run --help
