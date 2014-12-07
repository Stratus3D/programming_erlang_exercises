-module(geometry).
-export([area/1, perimeter/1]).

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side;
%% exercise called for additional clauses to compute the
%% area of circles and triangles.
area({circle, Radius}) -> (Radius * Radius) * math:pi();
area({triangle, Width, Height}) -> (Width * Height) / 2.

perimeter({rectangle, Width, Height}) -> Width * Height * 2;
perimeter({square, Side}) -> Side * Side * 2;
perimeter({circle, Radius}) -> (Radius * 2) * math:pi();
perimeter({triangle, Width, Height}) -> math:sqrt((Width * Width) + (Height * Height)).
