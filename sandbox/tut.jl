# @async begin
  counter = 1
  listen_s = listen(2000)
  while true
    println("waiting on socket")
    let s = accept(listen_s)
      println("conn accepted")
      # request = readall(s)
      # println(request);
      println("writing response $counter")
      write(s, "Golish Polish $counter")
      counter += 1
      close(s)
    end
  end
# end
