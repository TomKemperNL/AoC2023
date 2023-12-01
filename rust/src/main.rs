fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    // use super::*;

    #[test]
    fn test_divide() {
        assert_eq!(2,2);        
    }
}