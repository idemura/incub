#!/bin/sh
mongod --fork --dbpath=$HOME/mongo --logpath=$HOME/mongo/mongo.log --logappend
