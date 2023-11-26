use aoc2022::read_input;
use aoc2022::split_into_tuple;
use std::collections::HashSet;

fn main() {
    let lines = read_input("../inputs/day07/input.txt");

    let part_1 = part_1(&lines);
    std::println!("Part 1: {}", part_1);

    let part_2 = part_2(&lines);
    std::println!("Part 2: {}", part_2);
}

#[derive(Debug)]
struct FileSystem {
    fs: Vec<Directory>,
    current: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct Directory {
    name: String,
    files: Vec<File>,
    parent: Option<usize>,
    children: HashSet<usize>,
    size: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct File {
    size: usize,
    name: String,
}

impl FileSystem {
    fn new() -> Self {
        let root = Directory::new(String::from("/"), None);

        Self {
            fs: vec![root],
            current: 0,
        }
    }

    fn directory(&mut self, name: String, parent: Option<usize>) -> usize {
        //first see if it exists
        for directory in &self.fs[self.current].children {
            if self.fs[*directory].name == name {
                return *directory;
            }
        }
        // Otherwise, add new node
        let idx = self.fs.len();
        self.fs.push(Directory::new(name, parent));
        idx
    }

    fn cd(&mut self, param: String) {
        if param.contains("..") {
            self.current = self.fs[self.current].parent.unwrap();
            return;
        }
        let dir_idx = self.add_dir_to_current(param);
        self.current = dir_idx;
    }

    fn ls(&mut self, input: &[String]) {
        for line in input {
            if line.starts_with('$') {
                return;
            }

            let (a, name) = split_into_tuple::<String>(line, ' ');
            if a == "dir" {
                self.add_dir_to_current(name);
            } else {
                self.fs[self.current].add_file(name, a.parse::<usize>().unwrap());
            }
        }
    }

    fn add_dir_to_current(&mut self, name: String) -> usize {
        std::println!("name; {}", name);
        let dir_idx = self.directory(name, Some(self.current));
        let current = &mut self.fs[self.current];

        current.children.insert(dir_idx);

        dir_idx
    }

    fn size(&self, root: usize) -> (usize, usize) {
        // Get known size of current directory
        let mut dir_size = self.fs[root].size;
        let mut response = 0;
        for children in &self.fs[root].children {
            // Get size of all children
            let (child_dir_size, child_response) = self.size(*children);
            dir_size += child_dir_size;
            response += child_response;
        }

        if dir_size < 100000 {
            response += dir_size;
        }

        (dir_size, response)
    }

    fn file_size(&mut self) {
        for dir in &mut self.fs {
            dir.file_size();
        }
    }

    fn find_dir_over(&self, need_space: usize, root: usize) -> (usize, usize) {
        let mut dir_size = self.fs[root].size;
        let mut response = 0;

        for children in &self.fs[root].children {
            let (child_dir_size, child_response) = self.find_dir_over(need_space, *children);
            dir_size += child_dir_size;

            if child_response >= need_space && (response == 0 || child_response < response) {
                response = child_response;
            }
        }

        if dir_size >= need_space && (response == 0 || dir_size < response) {
            response = dir_size;
        }

        (dir_size, response)
    }
}

impl Directory {
    fn new(name: String, parent: Option<usize>) -> Self {
        Self {
            name,
            files: vec![],
            parent,
            children: HashSet::new(),
            size: 0,
        }
    }

    fn add_file(&mut self, name: String, size: usize) {
        self.files.push(File::new(name, size));
    }

    fn file_size(&mut self) {
        self.size = self.files.iter().map(|file| -> usize { file.size }).sum();
    }
}

impl File {
    fn new(name: String, size: usize) -> Self {
        Self { name, size }
    }
}

fn part_1(input: &[String]) -> u32 {
    let mut fs = parse_input(input);

    fs.file_size();
    let (_, response) = fs.size(0);

    response as u32
}

fn part_2(input: &[String]) -> u32 {
    // fs.calculateSize(fs.root)
    // used := fs.usedSpace(fs.root)
    // needSpace := 30_000_000 - (70_000_000 - used)
    // result := lib.MinInt64(fs.findDirOver(needSpace, fs.root))
    let mut fs = parse_input(input);

    fs.file_size();
    let (total, _) = fs.size(0);
    let need_space = 30_000_000 - (70_000_000 - total);
    let (_, response) = fs.find_dir_over(need_space, 0);

    response as u32
}

fn parse_input(input: &[String]) -> FileSystem {
    let mut fs = FileSystem::new();

    for (i, line) in input.iter().enumerate() {
        if line.starts_with('$') {
            let mut iter = line.strip_prefix("$ ").unwrap().split(' ');
            let cmd = iter.next().unwrap();
            let param = iter.next();

            match cmd {
                "cd" => fs.cd(String::from(param.unwrap())),
                "ls" => fs.ls(&input[i + 1..]),
                &_ => panic!(),
            }
        }
    }

    fs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_test() {
        let test_input = read_input("../inputs/day07/input.test");
        let expected_result = 95437;

        let result = part_1(&test_input);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn part_2_test() {
        let test_input = read_input("../inputs/day07/input.test");
        let expected_result = 24933642;

        let result = part_2(&test_input);

        assert_eq!(expected_result, result);
    }
}
