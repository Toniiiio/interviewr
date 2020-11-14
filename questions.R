options(stringsAsFactors = FALSE)
questions <- list()

questions[["Multiples of 3 and 5"]] <- list(
  text = "If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.",
  time = 300,
  hints = c("The modulus can be calculated in R with '%%'", "7/3 is 2 remaining one. 7%%3 is 1.", "Type in R: '6%%3'", "Type in R: '6%%3' & '6%%5'"),
  solution = 233168,
  answer = "sum = 0
            for(nr in 1:999){
                if(!nr%%5 | !nr%%3) sum = sum + nr
            }
            print(sum)"
)

questions[["FizzBuzz"]] <- list(
  text = 'Classical FizzBuzz: Print every number between 1 and 100, However for numbers that are multiples of 3 print "Fizz" and for numbers that are multiples of five print "Buzz."   For numbers that are multiples of both 3 and 5 print "FizzBuzz".',
  time = 300
)

questions[["Find prime numbers"]] <- list(
  text = "Sum of all prime numbers below 5000 that can be devided by 72",
  time = 120,
  solution = 0,
  answer = 0
)

questions[["Compute standard deviation from scratch"]] <- list(
  text = "Generate 100 random numbers with seed 50 and write a standard deviation function.",
  time = 600
)
