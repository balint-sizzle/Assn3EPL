#!/usr/bin/expect -f 
spawn rm -f RunRabbit.scala
spawn sbt package
expect {
  "Create a new server? y/n (default y)*" {
    send "y\n"
    interact
  } timeout {
    interact
  } eof {
    exit
  }
}
