#!/bin/sh
export GOPATH=$PWD
go install server && echo "Build SUCCESSFUL"
