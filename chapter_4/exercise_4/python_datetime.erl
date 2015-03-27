-module(python_datetime).

% Since I am not really familiar with Python's datetime class
% I had to spend some time browsing the documentation to figure
% out what Erlang lacked.
%
% Python's datetime methods and Erlang's equivalent function:
% Python                    Erlang
% __add__                   None
% __eq__                    ==
% __ge__                    >=
% __getattribute__          Not needed
% __gt__                    >
% __hash__                  Not needed
% __le__                    =<
% __lt__                    <
% __ne__                    /=
% __radd__                  Not needed
% __repr__                  Not needed
% __rsub__                  Not needed
% __str__                   Not needed
% __sub__                   -
