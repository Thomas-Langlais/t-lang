fn main() {
    // ownership is a feature in rust that ensures memory safety at compile time. very nifty stuff
    let str1 = String::from("hello");

    // the idea is that as soon as the variable is taken out of scope (aka here it gets sent to take_ownership)
    // it has been used as is no longer valid and cannot be used again because it's been taken off the stack.
    println!("{}", str1); // print the String
    take_ownership(str1); // move the String to the fn, which invalidates str1 here
    println!("{}", str1); // ERROR - str1 moved to the take_ownership scope, and cannot find it on the stack
}

fn take_ownership(text: String) {

}