## This is a draft
There may be small changes before this is fianalized

# Instructions
Due 6/21

## Getting Started
* Choose group members and a project name
* Follow this [Link](https://classroom.github.com/g/SvBy08dw) to create/Join a group repo.  Even if you are working alone.
* Like in HW0 you will need to `git clone` the new repository
  * ```cd``` into the newly created directory 
  * You always want to take advantage of the latest corrections to the assignments and shared tests so we will add the main repository as a source
    * In your terminal type ```git remote add upstream https://github.com/BU-CS320/Summer-2019-project.git```
    * check that it worked by typing ```git remote -v```.  You should see see the line ```upstream https://github.com/BU-CS320/Summer-2019-project.git (fetch)```
    * You always want to keep your assignment up to date by running ```git pull upstream master```, do that now
  * check the status of your repo: ```git status```
* fill you the readme.md
  
  
## Grading
### 20% on parser code and tests.
* write a generator and shrinker for a reasonable subset of your AST.
* a fully parenthesized function from your parser to a string.
* a fully pretty printing function from your parser to a string.
quick check tests that parse the output of those functions.


### 20% on Eval code and tests.
write unit tests that test all the features of your language.  It should be clear that you have reasonably working eval function by only looking at the tests.


### 60% do stuff in your language
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

We will allow radically different projects as long as you clear it with Mark at least a week before the deadline.  Cool options include: compilation, type inference/checking/dependent types, anything from previous semesters.
