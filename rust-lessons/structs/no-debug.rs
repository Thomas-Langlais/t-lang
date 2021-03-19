// if you remove the derive statement, it will not compile
#[derive(Debug)]
struct Rectangle {
    height: u32,
    width: u32
}

fn main() {
    /* we cannot print the struct because it does not implement the proper traits
    there are macros that do this for us, without us having to implement the
    std::fmt::Display trait. we can use the Debug format "{:?}" to print the contents of
    the struct but it NEEDS to derive Debug. */
    let rect = Rectangle {
        width: 2,
        height: 45
    };
    // print it
    println!("{:?}", rect);
}