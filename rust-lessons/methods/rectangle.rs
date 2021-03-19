
/* in rust, there exists a way to have similar syntax to OOP while keeping static typing
and that is by using the impl syntax.

This essentially enables us to bind function calls to struct with the impl type.

In this example, we use a rectangle to calculate it's area without defining a function.
a function follows the "fn func_name(args...) -> return_type {...}" syntax, but a method uses this.
"fn method_name(&self, ...args) -> return_type {...}", self is the type bound in the impl statement.
*/

struct Rectangle {
    height: u64,
    width: u64
}

impl Rectangle {
    /* we use a reference here because we almost never want to
    take ownership of the Rectangle - unless we do a permanent transformation
    and the rectangle should become invalidated. */
    fn area(&self) -> u64 {
        return self.height * self.width;
        // we can also return like this
        // self.height * self.width
        // the thing to note is the lack of ';' and no return keyword
    }
}

/* We can also have multiple impl blocks for the same struct, this can allow for different methods to be
available depending on what is installed.

Another cool thing that impl can have is binding Java "static" methods to a struct here is an example below */
impl Rectangle {
    fn square(size: u64) -> Rectangle {
        Rectangle {
            height: size,
            width: size
        }
    }
}

fn main() {
    // instantiate the rectangle
    let rect = Rectangle {
        height: 1,
        width: 1
    };

    /* this looks like it's using encapsulation, but it's just using the
    method declaration seperate from the struct declaration.*/
    let area = rect.area();
    println!("The area is: {}", area);

    // we create a rectangle from the Rectangle impl scope.
    let rect = Rectangle::square(3);
    println!("The area of the square is: {}", rect.area());
}