CREATE TABLE users ( 
    id UUID PRIMARY KEY, 
    username varchar(255), 
    email varchar(255), 
    password varchar(255)
);

CREATE TABLE codingproblems ( 
    id UUID PRIMARY KEY, 
    deadline timestamptz default (now() at time zone 'utc'), 
    problemTags text, 
    difficulty varchar(255), 
    title text, 
    description text, 
    templateCode text
); 

CREATE TABLE codingproblemcases (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    casesCodingProblemId UUID,
    description text,
    input text,
    output text,
    visibility varchar(255),
    CONSTRAINT fk_codingproblem
        FOREIGN KEY(casesCodingProblemId)
            REFERENCES codingproblems(id)
);

CREATE TABLE attempts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    userId UUID,
    codingProblemId UUID,
    submitted_on timestamptz default (now() at time zone 'utc'), 
    completed_on timestamptz default (now() at time zone 'utc'), 
    code text, 
    state text,
    CONSTRAINT fk_codingproblemattempt
        FOREIGN KEY(codingProblemId)
            REFERENCES codingproblems(id),
            
    CONSTRAINT fk_userattempt
        FOREIGN KEY(userId)
            REFERENCES users(id)
);



-- Dummy Data

INSERT INTO users (id, username, email, password)
VALUES ('f6736119-0993-4a2b-beb0-7a2fccef99a4', '123', 'test@mail.com', 'password'),
       ('f6736119-0993-4a2b-beb0-7a2fccef99a2', 'user', 'test@mail.com', 'password');

INSERT INTO codingproblems
VALUES ('f6736119-0993-4a2b-beb0-7a2fccef99a2', '2024-06-01 12:00:00.0', '', 'Easy', 'Optimize for loop', 'Optimize the while loop below. Currently it has a time complexity of O(n^2), but can be O(n).', '', 'module OptimizeLoop where \n import Data.Functor \n');