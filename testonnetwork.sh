#Main contract address : KT1J5mFCLJmAV3r8najMH8HmTFSwQdrToRFM

#Callback address (Nat) :  KT19JjUrQr7A4CM9FmwtcYJvXvTjguvi4XaS

# V2 :  KT1XEpf9GsZ7yxyyjRagxhipVdVTmp4Ttyv4

#====== Add an operator to start ==========================
#PARAM=`stack exec -- tzbtc addOperator --operator tz1VwXeEPw2tkTgDSUUbEb5fe63b24gNEssa`

#====== Mint some tokens to redeem address ===================
#PARAM=`stack exec -- tzbtc mint --to tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx --value 1000`

#============ Check balance in redeem address ========================
# Callback contrect KT19JjUrQr7A4CM9FmwtcYJvXvTjguvi4XaS
#PARAM=`stack exec -- tzbtc getBalance --address tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx --callback  KT19JjUrQr7A4CM9FmwtcYJvXvTjguvi4XaS`
# Output 1000

#================== Transfer some tokens to alice ===================================
#PARAM=`stack exec -- tzbtc transfer --from tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx --to tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ --value 120`

# Output: This transaction when called as `sidhu` failed with not enough allowance error. But worked
# when called as `john` who is the owner of the `from` account in txn

#================== Check balance in alice account =================================
#PARAM=`stack exec -- tzbtc getBalance --address tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ --callback  KT19JjUrQr7A4CM9FmwtcYJvXvTjguvi4XaS`
# Output : 120

#====================== Alice redeems 20 tokens ===================================
#----------------------- Transfer 20 tokens to redeem address
#PARAM=`stack exec -- tzbtc transfer --from tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ  --to tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx --value 20`
#----------------------- Check balance in alice
#PARAM=`stack exec -- tzbtc getBalance --address tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ --callback  KT19JjUrQr7A4CM9FmwtcYJvXvTjguvi4XaS`
#Output: 100
#----------------------- Burn 20 tokens
#PARAM=`stack exec -- tzbtc burn --value 20`
#output: Storage pair for (TotalSupply, (TotalBurned, TotalMinted)) = (Pair 980 (Pair 20 1000)))



#++++++++++++++++++++++++++++++++++ Migration Tests +++++++++++++++++++++++++++++++++++++++++

# Originated new version at address : KT1XEpf9GsZ7yxyyjRagxhipVdVTmp4Ttyv4

# Originate manager

#./alphanet.sh client originate contract migrationAgent for john \
#                                transferring 1 from john \
#                                running container:../../temp/agent.tz \
#                                --burn-cap 5.763 \
#                                --init 'Pair "KT1J5mFCLJmAV3r8najMH8HmTFSwQdrToRFM" "KT1XEpf9GsZ7yxyyjRagxhipVdVTmp4Ttyv4"'
#
#Address: KT1NFcsiaUzD1Lao4e6HxsExWLBbDtncA4oX


# Start migration in V1
# PARAM=`stack exec -- tzbtc startMigrateTo KT1NFcsiaUzD1Lao4e6HxsExWLBbDtncA4oX`

#Got error contract is not paused!

# Pause the contract
#PARAM=`stack exec -- tzbtc pause`

# Start migration in V1
# PARAM=`stack exec -- tzbtc startMigrateTo KT1NFcsiaUzD1Lao4e6HxsExWLBbDtncA4oX`


# Alice tries to migrate her tokens
#PARAM=`stack exec -- tzbtc migrate`

#Got error Operations are paused!

# Start migrateFrom in V2
#PARAM=`stack exec -- tzbtc startMigrateFrom KT1NFcsiaUzD1Lao4e6HxsExWLBbDtncA4oX`

# Alice tries to migrate her tokens
#PARAM=`stack exec -- tzbtc migrate`

#echo $PARAM
#./alphanet.sh client transfer 0 from alice to  KT1J5mFCLJmAV3r8najMH8HmTFSwQdrToRFM  --arg "$PARAM"
#Debug attempt 1 ---------------------------------
#./alphanet.sh client originate contract migrationAgent2 for john \
#                                transferring 1 from john \
#                                running container:../../temp/agent.tz \
#                                --burn-cap 5.763 \
#                                --init 'Pair "tz1MuPWVNHwcqLXdJ5UWcjvTHiaAMocaZisx" "KT1XEpf9GsZ7yxyyjRagxhipVdVTmp4Ttyv4"'

#KT1FwLk7ynQFRJ2HjsZYYohzTrPeReQmZSVN

#PARAM=`stack exec -- tzbtc mintForMigrations --to tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ --value 100`

#./alphanet.sh client transfer 0 from john to KT1FwLk7ynQFRJ2HjsZYYohzTrPeReQmZSVN  --arg "Pair 0x00004ca01f03b604ed516fee0295a4690786807e586b 20"

#PARAM=`stack exec -- tzbtc startMigrateFrom KT1FwLk7ynQFRJ2HjsZYYohzTrPeReQmZSVN`
#./alphanet.sh client transfer 0 from john to  KT1XEpf9GsZ7yxyyjRagxhipVdVTmp4Ttyv4 --arg "$PARAM"

# Get balance of alice in v2

#PARAM=`stack exec -- tzbtc getBalance --address tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ --callback  KT19JjUrQr7A4CM9FmwtcYJvXvTjguvi4XaS`
#./alphanet.sh client transfer 0 from john to KT1XEpf9GsZ7yxyyjRagxhipVdVTmp4Ttyv4 --arg "$PARAM"

# Debug Attempt 2

# Deploy V1 : KT1VK3dtSrPvTa3F3SZHF28UtZgyN6spQWcR
# Deploy V2 : KT1G3Vxm7yYauJkm8broz9uGt9rivcERaoXG
# Deploy Manager: KT19RkZ2f7QhGmpLThiWE1jKcv3KFywyTFZV
#        Initial storage:
#          (Pair "KT1VK3dtSrPvTa3F3SZHF28UtZgyN6spQWcR" "KT1G3Vxm7yYauJkm8broz9uGt9rivcERaoXG")
V1=KT1VK3dtSrPvTa3F3SZHF28UtZgyN6spQWcR
V2=KT1G3Vxm7yYauJkm8broz9uGt9rivcERaoXG
MANAGER=KT19RkZ2f7QhGmpLThiWE1jKcv3KFywyTFZV

# Add an operator to V1
#PARAM=`stack exec -- tzbtc addOperator --operator tz1VwXeEPw2tkTgDSUUbEb5fe63b24gNEssa `
#./alphanet.sh client transfer 0 from john to $V1  --arg "$PARAM"

# Mint some tokens for alice
#PARAM=`stack exec -- tzbtc mint --to tz1SdBs7PEc75PSg7Cyxq6sv2TgqSqRy1ZKJ --value 500`
#./alphanet.sh client transfer 0 from sidhu to $V1  --arg "$PARAM"

# Pause the contract V1
#PARAM=`stack exec -- tzbtc pause`
#./alphanet.sh client transfer 0 from sidhu to $V1  --arg "$PARAM"

# StartMigrateTo on V1
#PARAM=`stack exec -- tzbtc startMigrateTo KT19RkZ2f7QhGmpLThiWE1jKcv3KFywyTFZV`
#./alphanet.sh client transfer 0 from john to $V1  --arg "$PARAM" --burn-cap 5.763

# StartMigrateFrom on V2
#PARAM=`stack exec -- tzbtc startMigrateFrom KT19RkZ2f7QhGmpLThiWE1jKcv3KFywyTFZV`
#./alphanet.sh client transfer 0 from john to $V2 --arg "$PARAM" --burn-cap 5.763

# Unpause V1
#PARAM=`stack exec -- tzbtc unpause`
#./alphanet.sh client transfer 0 from john to $V1  --arg "$PARAM"

# Alice migrates on V1
PARAM=`stack exec -- tzbtc migrate`
./alphanet.sh client transfer 40 from alice to $V1  --arg "$PARAM" -q
