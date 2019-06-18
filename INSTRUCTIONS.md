## This is a draft

We will allow radically different projects as long as you clear it with Mark at least a week before the deadline.  Cool options include: compilation, type inference/checking/dependent types, anything from previous semesters.

get started:
* Choose group members and a project name
* Follow this link
* fill you the readme.md


20% on parser code and tests.
* write a generator and shrinker for a reasonable subset of your AST.
* a fully parenthesized function from your parser to a string.
* a fully pretty printing function from your parser to a string.
quick check tests that parse the output of those functions.


20% on Eval code and tests.
write unit tests that test all the features of your language.  It should be clear that you have reasonably working eval function by only looking at the tests.


60%
pick 6 problems from project Euler and write solutions in your language.
for instance if you follow the imperative hints you could solve https://projecteuler.net/problem=1 with the code that looks like
```
module euler1 {

  def main() {
    counter := 0;
    out := 0;
    
      while (counter < 1000){
        if (???) {
        out := out + counter;
      }
      
      counter := counter + 1;
    }
    
    return out;
  }

}
```
make sure you include your solutions in the example directory, and they are tested by your automated tests.


If you would like to solve problems in your language not included on project Euler, Mark must approve a week before the due date.