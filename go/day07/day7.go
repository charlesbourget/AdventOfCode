package main

import (
	"fmt"
	"strings"

	lib "github.com/charlesbourget/aoc-lib"
)

func main() {
	input, err := lib.Read("../inputs/day07/input.txt")
	if err != nil {
		panic(fmt.Errorf("error while reading input, %s", err))
	}

	fmt.Printf("Part 1: %d\n", Part1(input))
	fmt.Printf("Part 2: %d\n", Part2(input))
}

func Part1(input []string) int64 {
	fs := parseFs(input)
	fs.calculateSize(fs.root)

	result := fs.findTotal(100_000, fs.root)

	return result
}

func Part2(input []string) int64 {
	fs := parseFs(input)
	fs.calculateSize(fs.root)
	used := fs.usedSpace(fs.root)
	needSpace := 30_000_000 - (70_000_000 - used)
	result := lib.MinInt64(fs.findDirOver(needSpace, fs.root))

	return result
}

type Fs struct {
	current *Directory
	root    *Directory
}

type Directory struct {
	name      string
	files     []*File
	parent    *Directory
	childrens map[string]*Directory
	size      int64
}

type File struct {
	size int64
	name string
}

func parseFs(input []string) *Fs {
	root := Directory{
		name:      "/",
		files:     []*File{},
		parent:    nil,
		childrens: make(map[string]*Directory),
		size:      0,
	}

	fs := Fs{
		current: &root,
		root:    &root,
	}

	for i, v := range input {
		if !strings.HasPrefix(v, "$") {
			continue
		}

		cmd := strings.Split(strings.TrimPrefix(v, "$ "), " ")

		if cmd[0] == "cd" {
			fs.cd(cmd[1])
		} else if cmd[0] == "ls" {
			fs.ls(input[i+1:])
		}

	}

	return &fs
}

func (fs *Fs) cd(param string) {
	if param == ".." {
		fs.current = fs.current.parent
		return
	}

	value, ok := fs.current.childrens[param]
	if ok {
		fs.current = value
	} else {
		newDir := fs.current.mkdir(param)
		fs.current = newDir
	}

}

func (fs *Fs) ls(input []string) {
	for _, v := range input {
		if strings.HasPrefix(v, "$") {
			return
		}

		s, name := lib.Unpack(strings.Split(v, " "))

		if s == "dir" {
			fs.current.mkdir(name)
		} else {
			fs.current.touch(name, lib.ToInt64(s))
		}
	}
}

func (fs *Fs) calculateSize(dir *Directory) int64 {
	if dir.size != 0 {
		return dir.size
	}

	var size int64 = 0

	for _, v := range dir.files {
		size += v.size
	}

	for _, v := range dir.childrens {
		size += fs.calculateSize(v)
	}

	dir.size = size

	return size
}

func (fs *Fs) findTotal(threshold int64, dir *Directory) int64 {
	var total int64 = 0

	for _, v := range dir.childrens {
		total += fs.findTotal(threshold, v)
	}

	if dir.size <= threshold {
		total += dir.size
	}

	return total
}

func (fs *Fs) findDirOver(threshold int64, dir *Directory) []int64 {
	var dirs = []int64{}
	if dir.size >= threshold {
		dirs = append(dirs, dir.size)
	}

	for _, v := range dir.childrens {
		dirs = append(dirs, fs.findDirOver(threshold, v)...)
	}

	return dirs
}

func (fs *Fs) usedSpace(dir *Directory) int64 {
	return fs.root.size
}

func (dir *Directory) mkdir(name string) *Directory {
	newDir := &Directory{
		name:      name,
		files:     []*File{},
		parent:    dir,
		childrens: make(map[string]*Directory),
		size:      0,
	}

	dir.childrens[name] = newDir

	return newDir
}

func (dir *Directory) touch(name string, size int64) *File {
	newFile := &File{
		name: name,
		size: size,
	}

	dir.files = append(dir.files, newFile)

	return newFile
}
