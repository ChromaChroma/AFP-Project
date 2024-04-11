CREATE TABLE users ( 
    id UUID PRIMARY KEY, 
    username varchar(255), 
    email varchar(255), 
    password varchar(255)
);

CREATE TABLE codingproblems ( 
    id UUID PRIMARY KEY, 
    deadline timestamptz default (now() at time zone 'utc'), 
    problemTags varchar(255), 
    difficulty varchar(255), 
    title varchar(255), 
    description varchar(255), 
    testCases varchar(255), 
    templateCode varchar(255)
); 

CREATE TABLE codingproblemcases ( 
    codingproblem_id UUID PRIMARY KEY, 
    description varchar(255),
    input varchar(255),
    output varchar(255),
    visibility varchar(255),
    CONSTRAINT fk_codingproblem 
        FOREIGN KEY(codingproblem_id)
            REFERENCES codingproblems(id)
); 

-- Dummy Data

INSERT INTO users (id, username, email, password)
VALUES ('f6736119-0993-4a2b-beb0-7a2fccef99a4', '123', 'test@mail.com', 'password'),
       ('f6736119-0993-4a2b-beb0-7a2fccef99a2', 'user', 'test@mail.com', 'password');

INSERT INTO codingproblems
VALUES ('f6736119-0993-4a2b-beb0-7a2fccef99a2', '2024-06-01 12:00:00.0', '', 'Easy', 'Optimize for loop', 'Optimize the while loop below. Currently it has a time complexity of O(n^2), but can be O(n).', '', 'module OptimizeLoop where \n import Data.Functor \n');