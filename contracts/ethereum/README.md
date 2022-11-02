# Hardhat setup

cd contracts/src/main
npm init -y
npx truffle init
npm install --save-dev hardhat
npx hardhat
npm install @openzeppelin/contracts

# Compile contracts
npx hardhat compile

# To do
move project to root (out of src/main)

# References
- https://docs.openzeppelin.com/learn/developing-smart-contracts#compiling-solidity-source-code
- https://github.com/timt/sbt-npm/blob/master/src/main/scala/io/shaka/sbt/Npm.scala
