# Language documentation

## Variables 

There are three main ways of declaring variables. **const**, **var** and **let**. The kind of declaration used reflects 
where the variable value ends up in memory. If the variable is a **const** it will be resolved at compile time. A 
**var** will  have its value stored in static memory and **let** is stored on the stack.

### NOTE!!!

Both **var** and **const** are only supported in a module scope. Functions can not have **const** or **var** statements.