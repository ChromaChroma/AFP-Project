-- Scheme

CREATE TABLE users (
    id UUID PRIMARY KEY,
    username varchar(255),
    email varchar(255),
    password varchar(255)
); 

CREATE TABLE codingproblems (
    id UUID PRIMARY KEY,
    deadline timestamp default (now() at time zone 'utc'),
    problemTags varchar(255),
    difficulty varchar(255),
    title varchar(255),
    description varchar(255),
    testCases varchar(255),
    templateCode varchar(255)
); 


-- Dummy Data

INSERT INTO users (id, username, email, password)
VALUES ('f6736119-0993-4a2b-beb0-7a2fccef99a4', '123', 'test@mail.com', 'password');

INSERT INTO codingproblems
VALUES ('f6736119-0993-4a2b-beb0-7a2fccef99a2', '2024-06-01 12:00:00.0', '', 'Easy', 'Optimize for loop', 'Optimize the while loop below. Currently it has a time complexity of O(n^2), but can be O(n).', '', 'module OptimizeLoop where \n import Data.Functor \n');