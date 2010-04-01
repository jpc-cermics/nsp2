// -*- Mode: scilab -*-
// Copyright (C) 2010 B. Pincon Iecn/Esial

// some tests for ndind2ind

//A = [1, 4, 7, 10;...
//     2, 5, 8, 11;...
//     3, 6, 9, 12];

ind = ndind2ind([3,4],[1,3],[2,4]);
if ~ind.equal[[4,6,10,12]] then, pause, end

ind = ndind2ind([3,4],[1,3],[2,4],ind_type="int");
if ~ind.equal[m2i([4,6,10,12])] then, pause, end

ind = ndind2ind([3,4],[2,3],[3,1]);
if ~ind.equal[[8,9,2,3]] then, pause, end

ind = ndind2ind([3,4],[2,3],[3,1],ind_type="int");
if ~ind.equal[m2i([8,9,2,3])] then, pause, end


//
//       [13, 16, 19, 22; 
//        14, 17, 20, 23;
//        15, 18, 21, 24; ]]
//
//A = [[1, 4, 7, 10;
//      2, 5, 8, 11;
//      3, 6, 9, 12];
//
//
ind = ndind2ind([3,4,2],[1,3],[2,4],[1,2]);
if ~ind.equal[[4,6,10,12,16,18,22,24]] then, pause, end

ind = ndind2ind([3,4,2],[1,3],[2,4],[1,2],ind_type="int");
if ~ind.equal[m2i([4,6,10,12,16,18,22,24])] then, pause, end

ind = ndind2ind([3,4,2],[1,3],[2,4],[2,1])
if ~ind.equal[[16,18,22,24,4,6,10,12]] then, pause, end

ind = ndind2ind([3,4,2],[1,3],[2,4],[2,1],ind_type="int")
if ~ind.equal[m2i([16,18,22,24,4,6,10,12])] then, pause, end



