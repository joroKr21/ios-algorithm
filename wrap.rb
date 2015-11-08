#!/usr/bin/env ruby

def exitWith(status)
  exit status.nil? ? 0 : status.exitstatus
end

def waitForChild
  Process.wait
  exitWith $?
end

def runInBackground(command)
  pid = fork do
    `#{command}`
    exitWith $?
  end

  trap :CHLD do
    waitForChild
  end

  loop do
    sleep 10
    Process.kill(0, pid)
    print '.'
  end
rescue Errno::ESRCH
  waitForChild
end

runInBackground ARGV.join(' ')
