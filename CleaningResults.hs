-- this file shows you the output from various runnings of cleaning data and counting votes

-- section 1 - Cleaning votes
-- We are using the following files: 
    --- sampleuk1.csv  - simple file with 10 votes. No errors, no blank lines
    --- sampleuk2.csv  - 10 votes with a blank line after candidate 5
    --- sampleuk3.csv  - 10 votes where there is a vote where there is duplicate preferences
    --- sampleuk4.csv  - 10 votes where there is a vote where there is a missing preference (i.e. non-contiguous)
    --- sampleuk5.csv  - 10 votes where there are votes that have duplicate preferences, missing preferences)
    --- sampleuk6.csv  - 10 votes where there is a vote that has no preference listed (i.e. all "*")

    ---- sampleuk1.csv   - simple file with 10 votes. No errors, no blank lines - the result of cleaning this should be : 
    [["D. Abbott"],
    ["D. Milliband","E. Milliband","A. Burbhm","E. Balls","D. Abbott"],
    ["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
    ["A. Burbhm","D. Milliband","E. Milliband"],
    ["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
    ["D. Milliband","E. Milliband"],
    ["E. Balls","E. Milliband","D. Milliband","A. Burbhm","D. Abbott"],
    ["E. Balls","D. Milliband","A. Burbhm","E. Milliband","D. Abbott"],
    ["E. Milliband","D. Milliband","E. Balls","A. Burbhm"],
    ["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"]  ]

    -- sampleuk2.csv  - 10 votes with a blank line after candidate 5 - the result of cleaning this should be (note the same as above): 
    [["D. Abbott"],
    ["D. Milliband","E. Milliband","A. Burbhm","E. Balls","D. Abbott"],
    ["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
    ["A. Burbhm","D. Milliband","E. Milliband"],
    ["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
    ["D. Milliband","E. Milliband"],
    ["E. Balls","E. Milliband","D. Milliband","A. Burbhm","D. Abbott"],
    ["E. Balls","D. Milliband","A. Burbhm","E. Milliband","D. Abbott"],
    ["E. Milliband","D. Milliband","E. Balls","A. Burbhm"],
    ["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"]]

 --- sampleuk3.csv  - 10 votes where there is a vote where there is duplicate preferences (in first vote) 
 -- the result of cleaning this should be

[["D. Abbott","E. Balls"],                --- difference here (duplicated deleted)
["D. Milliband","E. Milliband","A. Burbhm","E. Balls","D. Abbott"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"], 
["A. Burbhm","D. Milliband","E. Milliband"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
["D. Milliband","E. Milliband"],
["E. Balls","E. Milliband","D. Milliband","A. Burbhm","D. Abbott"],
["E. Balls","D. Milliband","A. Burbhm","E. Milliband","D. Abbott"],
["E. Milliband","D. Milliband","E. Balls","A. Burbhm"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"]]

--- sampleuk4.csv  - 10 votes where there is a vote where there is a missing preference (i.e. non-contiguous) (in first vote)
-- the result of cleaning this should be

[["D. Abbott","A. Burbhm"],             -- difference here (all after missing prefereence deleted)
["D. Milliband","E. Milliband","A. Burbhm","E. Balls","D. Abbott"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
["A. Burbhm","D. Milliband","E. Milliband"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
["D. Milliband","E. Milliband"],
["E. Balls","E. Milliband","D. Milliband","A. Burbhm","D. Abbott"],
["E. Balls","D. Milliband","A. Burbhm","E. Milliband","D. Abbott"],
["E. Milliband","D. Milliband","E. Balls","A. Burbhm"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"]]

--- sampleuk5.csv  - 10 votes where there are votes that have duplicate preferences, missing preferences) (first two votes changed) 
-- the result of cleaning this should be
[["D. Abbott","E. Balls"],              --- difference here (duplicated deleted)
["D. Milliband","E. Milliband"],        -- difference here (all after missing prefereence deleted)
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
["A. Burbhm","D. Milliband","E. Milliband"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
["D. Milliband","E. Milliband"],
["E. Balls","E. Milliband","D. Milliband","A. Burbhm","D. Abbott"],
["E. Balls","D. Milliband","A. Burbhm","E. Milliband","D. Abbott"],
["E. Milliband","D. Milliband","E. Balls","A. Burbhm"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"]]

--- sampleuk6.csv  - 10 votes where there is a vote that has no preference listed (i.e. all "*") (first vote changed)
-- the result of cleaning this should be (note first vote has been scrapped - only 9 votes appear)

[["D. Milliband","E. Milliband"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
["A. Burbhm","D. Milliband","E. Milliband"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"],
["D. Milliband","E. Milliband"],
["E. Balls","E. Milliband","D. Milliband","A. Burbhm","D. Abbott"],
["E. Balls","D. Milliband","A. Burbhm","E. Milliband","D. Abbott"],
["E. Milliband","D. Milliband","E. Balls","A. Burbhm"],
["D. Milliband","E. Milliband","E. Balls","A. Burbhm","D. Abbott"]]
