Fact <- function(n) if (n == 1) 1 else n * Fact(n - 1)
Fact(5)

data("AirPassengers")
AP <- AirPassengers
AP
class(AP)
start(AP)
end(AP)
frequency(AP)
