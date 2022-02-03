% umut deniz sener
% 2018400225
% compiling: yes
% complete: yes

% include the knowledge base
:- ['load.pro'].

	%to substract elements of one list from the other and take square 
	substract_List([],[],[]).
	substract_List([H1|T1],[H2|T2],Result):-
	(Result = [H3|T3],
	(not(H1=(-1);H2=(-1)),substract_List(T1,T2,T3),H3 is (H1-H2)**2)),!;
	(substract_List(T1,T2,Result)).

	%to sum all elements in a list
	sum_List([],0).
	sum_List([H1|T1],Result) :-
		sum_List(T1,TailResult), Result is (TailResult + H1). 
		
		
	%to substract elements of one list from the other and take square and multiply with numbers in another list
	sub_mult_List([],[],[],[]).
	sub_mult_List([H1|T1],[H2|T2],[H3|T3],Result):-
	(Result=[H4|T4],(sub_mult_List(T1,T2,T3,T4),not(H1=(-1);H2=(-1);H3=(-1)),H4 is (H3*((H1-H2)**2)))),!;
	(sub_mult_List(T1,T2,T3,Result)).
	
	%concatenate 2 lists
	concatenate([],List,List).
	concatenate([H1|T1],List2,Result) :- 
			not(member(H1,List2)),concatenate(T1,List2,TailResult),Result = [H1|TailResult],!;
			member(H1,List2),concatenate(T1,List2,TailResult),Result = TailResult,!.
		
			
	%to find a list that contains Name1's old relations
	oldrelation(Name1,List):-
	findall(R,(old_relation([Name1,R]);old_relation([R,Name1])),List).
	
	%to find length of a list
	length_of([],0).
	length_of(List, Length) :- [_|Tail] = List, 
					length_of(Tail, TailLength), 
					Length is TailLength+1.
	
	%to find whether intersection between NamesActivities and TargetActivities less than 3
	intersection(NamesActivities,TargetActivities):-
	findall(R, (member(R,NamesActivities),member(T,TargetActivities),R=T),List),length_of(List,Length),Length<3.
	
	%to find whether Name1's in the tolerance limits of Name2's.
	limitChecker([],[]).
	limitChecker([H1|T1],[H2|T2]):-
	(H2=[], limitChecker(T1,T2),!);
	(H2 = [H3|T3], T3 = [H4|_], ((H3<H1),(H1<H4)), limitChecker(T1,T2)).
	
	%to find avg weighted_glanian_distance of Name1 and Name2
	avg_weighted_glanian_distance(Name1,Name2,Distance):-
	weighted_glanian_distance(Name1,Name2,R1),weighted_glanian_distance(Name2,Name1,R2),Distance is (R1+R2)/2.
	
% 3.1 glanian_distance(Name1, Name2, Distance) 5 points
	glanian_distance(Name1, Name2, Distance):-
	expects(Name1,_,Z1),glanian(Name2,_,Z2),
	substract_List(Z1,Z2,Result), sum_List(Result,R1), Distance is sqrt(R1),!.



% 3.2 weighted_glanian_distance(Name1, Name2, Distance) 10 points
	  weighted_glanian_distance(Name1,Name2,Distance):-
	  expects(Name1,_,Z1),glanian(Name2,_,Z2),weight(Name1,WeightList),
	 sub_mult_List(Z1,Z2,WeightList,R1),sum_List(R1,Final), Distance is sqrt(Final),!.
	  
	  
	  
% 3.3 find_possible_cities(Name, CityList) 5 points
	find_possible_cities(Name,CityList):-
	likes(Name, _, LikedCities),
	findall(CityName, 
	(
	(city(CityName,HabitantList,_), member(Name,HabitantList))
	),CityListt),
	concatenate(CityListt,LikedCities,CityList).
	
			
	
% 3.4 merge_possible_cities(Name1, Name2, MergedCities) 5 points
	merge_possible_cities(Name1, Name2, MergedCities):- 
	find_possible_cities(Name1,CityList1),find_possible_cities(Name2,CityList2),concatenate(CityList1,CityList2,MergedCities).
			
	
% 3.5 find_mutual_activities(Name1, Name2, MutualActivities) 5 points
	find_mutual_activities(Name1, Name2, ActivityList):-
	likes(Name1,LikedActivities1,_),likes(Name2,LikedActivities2,_),findall(MutualActivities,(member(MutualActivities,LikedActivities1),member(MutualActivities,LikedActivities2)),ActivityList).


% 3.6 find_possible_targets(Name, Distances, TargetList) 10 points	
%first I find the necessary information about Name and check whether gender of each glanian in TargetList is in ExpectedGenders of Name 
%then i use findall predicate to find all Distance-Target pair and i use sort predicate for sorting Distance-Target pairs in list 
%then i decompose all Distance and Target pairs with findall predicate
	find_possible_targets(Name,Distance,TargetList):-
	expects(Name,GenderList,_),
	findall(Dist-Name1,(glanian(Name1,GlanianGender,_),member(GlanianGender,GenderList),glanian_distance(Name,Name1,Dist)),UnsortedTargetList),
	sort(UnsortedTargetList,All),findall(Dist, (member(Dist-_,All)),Distance),
	findall(Target, (member(_-Target,All)),TargetList).


% 3.7 find_weighted_targets(Name, Distances, TargetList) 15 points
%first I find the necessary information about Name and check whether gender of each glanian in TargetList is in ExpectedGenders of Name 
%then i use findall predicate to find all Distance-Target pair and i use sort predicate for sorting Distance-Target pairs in list 
%then i decompose all Distance and Target pairs with findall predicate
	find_weighted_targets(Name,Distances,TargetList):-
	expects(Name,GenderList,_),findall(Dist-Name1,(glanian(Name1,GlanianGender,_),
	member(GlanianGender,GenderList),weighted_glanian_distance(Name,Name1,Dist)),UnsortedTargetList),
	sort(UnsortedTargetList,All),findall(Dist, (member(Dist-_,All)),Distances),
	findall(Target, (member(_-Target,All)),TargetList).
	
	
% 3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets) 20 points
%first I find the necessary information about Name and check whether Target comply with criterias and find a TargetList
%then i find Activity-CityName-Target-Distance quaternary that satisfies criterias and i decompose them


	 find_my_best_target(Name, Distances, Activities, Cities, Targets):-
	
	
	oldrelation(Name,RelationList),expects(Name,GenderList,_),
	likes(Name, LikedActivities, LikedCities),dislikes(Name,DislikedActivities,
	DislikedCities, Limits),
	
	
	findall(Target, (
	glanian(Target,TargetGender,TargetFeatures),
	not(member(Target,RelationList)),	
	(member(TargetGender,GenderList)),
	(limitChecker(TargetFeatures,Limits)),
	(likes(Target,TargetLiked,_),intersection(DislikedActivities,TargetLiked))
	),
	Targetss),
	
	findall(Dist, (member(Target,Targetss),weighted_glanian_distance(Name,Target,Dist)),Dists),	
	sort(Dists,SortedDists),findall(Target, (member(Dist,SortedDists),weighted_glanian_distance(Name,Target,Dist)),Targetsx),
	
	
	findall(Activity-CityName-Target-Distance, 
	(member(Target,Targetsx),weighted_glanian_distance(Name,Target,Distance),
	city(CityName,HabitantList,ActivityList),member(Activity,ActivityList),
	((member(Name,HabitantList);member(CityName,LikedCities));(member(Activity,ActivityList),member(Activity,LikedActivities),not((member(Name,HabitantList);member(CityName,LikedCities))))),
	not(member(Activity,DislikedActivities)),
	not(member(CityName,DislikedCities)),
	(merge_possible_cities(Name,Target,CityList),member(CityName,CityList))
	),ActivitiesCities)
	,
	findall(City,
	(member(_-City-_-_,ActivitiesCities)
	), Cities),
	findall(Acts,
	(member(Acts-_-_-_,ActivitiesCities)
	), Activities),
	
	findall(Target,
	(member(_-_-Target-_,ActivitiesCities)
	), Targets),
	
	findall(Dist,
	(member(_-_-_-Dist,ActivitiesCities)),
	Distances)	.
	
	

% 3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets) 25 points
%first I find the necessary information about Name and check whether Target comply with criterias and find a TargetList
%then i find Activity-CityName-Target-Distance quaternary that satisfies criterias and i decompose them
	find_my_best_match(Name, Distances, Activities, Cities, Targets):-
	
	
	oldrelation(Name,RelationList),expects(Name,GenderList,_),
	likes(Name, LikedActivities, LikedCities),dislikes(Name,DislikedActivities,
	DislikedCities, Limits),glanian(Name,NameGender,NameFeature),
	
	findall(Target, (
	glanian(Target,TargetGender,TargetFeatures),dislikes(Target,TargetDislikedActivites,TargetDislikedCities,TargetLimits),
	expects(Target,TargetGenderList,_),
	not(member(Target,RelationList)),	
	(member(NameGender,TargetGenderList)),
	(member(TargetGender,GenderList)),
	(limitChecker(TargetFeatures,Limits)),
	(limitChecker(NameFeature,TargetLimits)),
	(likes(Target,TargetLiked,_),intersection(DislikedActivities,TargetLiked)),
	(intersection(TargetDislikedActivites,LikedActivities))
	),
	Targetsx),
	
	findall(Activity-CityName-Target-Distance, 
	(member(Target,Targetsx),avg_weighted_glanian_distance(Name,Target,Distance),
	city(CityName,HabitantList,ActivityList),member(Activity,ActivityList),
	likes(Target,TargetLikedActivities,TargetLikedCities),
	dislikes(Target,TargetDislikedActivites,TargetDislikedCities,TargetLimits),
	((member(Name,HabitantList);member(CityName,LikedCities));(member(Activity,ActivityList),member(Activity,LikedActivities),
	not((member(Name,HabitantList);member(CityName,LikedCities))))),
	
	((member(Target,HabitantList);member(CityName,TargetLikedCities));(member(Activity,ActivityList),member(Activity,TargetLikedActivities),
	not((member(Target,HabitantList);member(CityName,TargetLikedCities))))),
	
	not(member(Activity,DislikedActivities)),
	not(member(Activity,TargetDislikedActivites)),
	not(member(CityName,DislikedCities)),
	not(member(CityName,TargetDislikedCities)),
	(merge_possible_cities(Name,Target,CityList),member(CityName,CityList))
	),ActivitiesCities),
	
	findall(City,
	(member(_-City-_-_,ActivitiesCities)
	), Cities),
	findall(Acts,
	(member(Acts-_-_-_,ActivitiesCities)
	), Activities),
	
	findall(Target,
	(member(_-_-Target-_,ActivitiesCities)
	), Targets),
	
	findall(Dist,
	(member(_-_-_-Dist,ActivitiesCities)),
	Distances)
	.