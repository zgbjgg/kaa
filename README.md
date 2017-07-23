# kaa
![Kaa](https://user-images.githubusercontent.com/1471055/28333979-4c770d16-6bbf-11e7-90b1-a394d6e7e414.png)

**KAA** - support to execute jun tasks using protocol buffers

[![Build Status](https://travis-ci.org/zgbjgg/kaa.svg?branch=master)](https://travis-ci.org/zgbjgg/kaa)
[![Codecov](https://img.shields.io/codecov/c/github/zgbjgg/kaa.svg)](https://codecov.io/gh/zgbjgg/kaa)
[![License: MIT](https://img.shields.io/github/license/zgbjgg/kaa.svg)](https://raw.githubusercontent.com/zgbjgg/kaa/master/LICENSE)

KAA is a library written in erlang to send and receive tasks to a `JUN` worker through a protocol buffers, so then any client implementing the proto files can send messages to `JUN`.

This project is under development and should not be used in production, it's not ready for that.

### Creating the environment

To create a new kaa worker (that also creates a jun worker onto it):

```erlang
(kaa@hurakann)1> {ok, Key} = kaa_main_worker:start_link().
14:28:02.689 [info] initialized default modules for py pid <0.422.0>
14:28:03.042 [info] initialized jun worker pid <0.421.0>
{ok,<<"ngioehzlskpwopeypenolaztsalgerupwlnimzzxxtfwdvcfatevctjogkdhmzxatjljzarvgavmjhlciwfvuastmkyqctkpffmv">>}
```

This will create and hold the environment for kaa and jun, so kaa can now receive messages encoded into a protocol buffers and pass to the jun worker, receive the response and delivered as proto message.

### Building a proto messages

Now we are executing some tasks into jun, but using the proto messages in kaa format, so for example, let's generate a message to read a csv into dataframe:

```erlang
(kaa@hurakann)2> ReadCsvInstruction = kaa_proto:read_csv_instruction("<0.421.0>", "/Users/zgbjgg/jun-nogit/file.csv").    
<<8,0,16,0,26,9,60,48,46,52,50,49,46,48,62,10,32,47,85,
  115,101,114,115,47,122,103,98,106,103,...>>
```

As you can see, the returned value is a binary in a protocol buffers format (for kaa), so this can be decoded using the proto file. Let's send the proto message to the kaa worker:

```erlang
(kaa@hurakann)3> {ok, Result} = kaa_main_worker:kaa_proto_in(Key, ReadCsvInstruction).
{ok,<<10,2,111,107,26,234,6,131,104,3,100,0,15,36,101,
      114,108,112,111,114,116,46,111,112,97,113,117,...>>}
```

Now we have a `Result` variable with a proto message in a binary format, let's decode the message (this can be done by any client):

```erlang
(kaa@hurakann)4> rr("/Users/zgbjgg/kaa/include/kaa_result.hrl").
['KaaResult']
(kaa@hurakann)5> KaaResult = kaa_result:decode_msg(Result, 'KaaResult'). 
#'KaaResult'{ok = "ok",
             result = {dataframe,<<131,104,3,100,0,15,36,101,114,108,
                                   112,111,114,116,46,111,112,97,113,
                                   117,101,100,0,6,...>>}}
```

### Working with dataframes

Now we have a decoded result, the dataframe read by the read_csv function, lets play with the dataframe, first to all get only the dataframe value:

```erlang
(kaa@hurakann)6> {dataframe, DataFrame} = KaaResult#'KaaResult'.result. 
{dataframe,<<131,104,3,100,0,15,36,101,114,108,112,111,
             114,116,46,111,112,97,113,117,101,100,0,6,
             112,121,116,...>>}
```

Generate another instruction, this time we are going to sum all values of column age from dataframe:

```erlang
(kaa@hurakann)7> SumAgeInstruction = kaa_proto:sum_age_instruction("<0.421.0>", DataFrame, sum, "age"). 
<<8,0,16,1,26,9,60,48,46,52,50,49,46,48,62,18,242,6,10,
  234,6,131,104,3,100,0,15,36,101,...>>
```

We have now the sum instruction into a proto message format, let's send to kaa and finally parse the result from proto result message:

```erlang
(kaa@hurakann)8> {ok, Result2} = kaa_main_worker:kaa_proto_in(Key, SumAgeInstruction).                 
{ok,<<10,2,111,107,8,13>>}
(kaa@hurakann)9> kaa_result:decode_msg(Result2, 'KaaResult').   
#'KaaResult'{ok = "ok",result = {inumber,13}}
```

Ready!, if check the result we have an inumber with a value of 13 which is the correct sum of all columns!

### See also

[JUN: python pandas support for dataframes manipulation over erlang](https://github.com/zgbjgg/jun)

### Authors

@zgbjgg Jorge Garrido <zgbjgg@gmail.com>



