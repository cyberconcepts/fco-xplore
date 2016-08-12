#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-} 

-- |
--
-- see http://www.scs.stanford.edu/16wi-cs240h
--

import Turtle

-- main = echo "Hello, world!"

main = pwd >>= datefile >>= print
