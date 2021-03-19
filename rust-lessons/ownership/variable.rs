fn main() {
    // we instantiate a string
    let s1 = String::from("hello");
    // we move s1 to s2, this invalidates s1 because it's stack data is moved to s2.
    // this is how rust enforces memory safety at compile time.
    let s2 = s1;
    // if we try to use s1 we will get an error stating the stack data has been moved to s1
    println!("{}", s1);
}