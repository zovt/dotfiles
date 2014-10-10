# Self Review Questions
## 2.14
Narrowing conversions can make the result more inaccurate
## 2.15
An enumerated type allows you to specify valid values for the type
## 2.16
The new operator returns a reference to a newly created object
## 2.17
Autoboxing
## 2.18
A collection of related classes
## 2.19
Certain things are autoimported
## 2.20
It is a function

# Multiple Choice
## 2.7
b
## 2.8
c
## 2.9
d
## 2.10
b

# True/False
## 2.7
true
## 2.8
true

# Short Answer
## 2.10
```
double num3 = Math.sqrt(num2+num1);
```
## 2.11
```
Math.abs(total)
```
## 2.12
### a
0-19
### b
1-8
### c
10-54
### d
-50-49
# Programming Projects
## 2.8
```
public class 
## 2.9
```
import java.util.Scanner;
import java.text.DecimalFormat;

public class surfacevolume {
    static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        final double PI = 3.14159;
        DecimalFormat fmt = new DecimalFormat("0.####");
        double radius = input.nextDouble();
        double volume = 4/3*PI*radius*radius*radius;
        double surface = 4*PI*radius*radius;
        System.out.println(fmt.format(volume));
        System.out.println(fmt.format(surface));
    }
}
```
## 2.10
## 2.11
## 2.12
## 2.13
