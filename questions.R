options(stringsAsFactors = FALSE)
questions <- list()

questions[["Multiples of 3 and 5"]] <- data.frame(
  text = "If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.",
  time = 300,
  solution = 233168,
  answer = "sum = 0
            for(nr in 1:999){
                if(!nr%%5 | !nr%%3) sum = sum + nr
            }
            print(sum)"
)

questions[["FizzBuzz"]] <- data.frame(
  text = 'Classical FizzBuzz: Print every number between 1 and 100, However for numbers that are multiples of 3 print "Fizz" and for numbers that are multiples of five print "Buzz."   For numbers that are multiples of both 3 and 5 print "FizzBuzz".',
  time = 300
)

questions[["Find prime numbers"]] <- data.frame(
  text = "Sum of all prime numbers below 5000 that can be devided by 72",
  time = 120,
  solution = 0,
  answer = 0
)

questions[["Compute standard deviation from scratch"]] <- data.frame(
  text = "Generate 100 random numbers with seed 50 and write a standard deviation fct.",
  time = 600
)

