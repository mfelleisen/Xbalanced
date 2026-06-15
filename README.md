# Xbalanced 

a benchmark 

## Executables 

- xbalance is the original script turned into a program 

- x-fake-server is really just a fake web server. Its answers
  simulate the APIs that the quotes library expects. To make 
  sure it is compatible with the actual API, I have made sure not to
  change "qyotes,rkt" other than taking out the API URLs and API keys. 
  (The fake server sends back random prices for stocks, but fixed ones
  for mutual funds and etfs). 
  
- xrun is the benchmark entry point. 


## Original Usage 

```
./xbalance 
``` 
retrieves the "current price" for securities from the 
fake server, assess the value of securities and cash-like accounts, 
and displays those both as readable S-expressions and as HTML. 

The `xbalance` script could be re-directed to connect to actual APIs
by replacing "api-keys-public.rkt" with my private version. 

## Benchmark Usage 

```
./xrun
``` 

is the easiest way to run the benchmark. It spawns the `x-fake-server`
to get stock and fund quotes, runs the `xbalance` client against it,
and finally opens a web browser.

Alternatively, spawn the fake server first like this:

```
./x-fake-server &
```

and then run the original client. 

**Warning** If you do this, don't forget to tear down the server so
that the TCP port gets freed up again. 

## Results 

The resulting files are deposited in the Checks/ directory. 

The most recent html file is opened via the browser library. 
If you want to disable this behavior, comment out the last 
line of `print-html` in "xbalance".  (The web page does give you 
a good idea whether the script runs properly. Except for the first
table, the others are deterministic.)


## Data Files 

- data.ss is a grown Racket program that really just reflects all
  purchases, dividends, sales, donations, etc of securities. It also
  contains calls to compute the value of cash-like account. The latter
  have been replaced with plain amounts to decouple this benchmark
  from the Xmanage one. 
  
  This is a grown data file. Unlike for xmanage, I wanted to record
  the data in a program file. I did not expect to keep this file for
  30+ years. What is now visible is a vastly shortened, distorted
  version (See comments in the contract for the data from this file.) 
  
- api-keys-public.rkt -- just that

## Program Files for Balancing 

- balance is the actual script for assessing the value of my
  securities and accounts. 

- assessment.rkt is a data representation for assessments. 

- render-as-sexpr.rkt .. as the name says 

- render-as-html.rkt .. as the name says, but watch! It uses OO just
  for the heck of it ... 
  
- render-index.rkt .. as the name says, it creates an index for all
  HYML files

- representations.rkt describes the data file with contracts and
  provides basic access functions based on these contracts. 
  
## Library Files 

- dates.rkt turns dates into file names and vice versa 

- quotes.rkt provides services for retrieving current stock/mutual
  fund/etf prices from web APIs, including ways to keep it cheaper. 
  It's a library file that I used for other things to, but here it is
  modified (API keys, urls) to work with the fake server. 

- render-common.rkt shared constants 

- common.rkt shared delivers constants shared between the server and
  the client.  

