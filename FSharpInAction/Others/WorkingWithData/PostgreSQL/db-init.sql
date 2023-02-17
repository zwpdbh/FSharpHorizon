-- psql -h localhost -U postgres -a -f "<script name>"
-- Database: fsharp

-- DROP DATABASE IF EXISTS fsharp;
drop table if exists Course; 

create table Course (
   Id serial primary key,
   Name varchar (50) NOT NULL
);

drop table if exists Student ;
create table Student (
	Id serial primary key,
	Name varchar (50) NOT NULL,
	Age int 
);

drop table if exists CourseSelection;
create table CourseSelection (
	Id serial primary key,
	StudentId int references Student(id) on delete cascade,
	CourseId int references Course(id) on delete cascade,
);

drop table if exists LastStudent;
create table LastStudent (
	Id int primary key,
	Name varchar (50) NOT NULL,
	Age int 
);
