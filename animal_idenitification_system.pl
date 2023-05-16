/* Animal to be tested */
animal(cheetah):- cheetah, !.
animal(tiger):- tiger, !.
animal(giraffe):- giraffe, !.
animal(zebra):- zebra, !.
animal(ostrich):- ostrich, !.
animal(penguin):- penguin, !.
animal(albatross):- albatross, !.
animal(deer):- deer , !.
animal(peacock) :- peacock, !.
animal(elephant) :- elephant, !.

/* animal identification rules */
cheetah :- mammal,
           carnivore,
           verify(has_tawny_color),
           verify(has_dark_spots).
tiger :- mammal,
         carnivore,
         verify(has_tawny_color),
         verify(has_black_stripes).
giraffe :- ungulate,
           verify(has_long_neck),
           verify(has_long_legs).
zebra :- ungulate,
         verify(has_black_stripes).

ostrich :- bird,
           verify(does_not_fly),
           verify(has_long_neck).
penguin :- bird,
           verify(does_not_fly),
           verify(swims),
           verify(is_black_and_white).
albatross :- bird,
             verify(appears_in_story_Ancient_Mariner),
             verify(flys_well).
deer :- herbivorous,
        ungulate,
        verify(has_horn),
        verify(is_reddish_brown_coast).
peacock :- bird,
           verify(has_long_neck),
           verify(is_blue_and_green),
           verify(has_eye_on_feather).
elephant :- mammal,
            herbivorous,
            verify(has_big_size),
            verify(has_long_trunks_and_tusks),
            verify(has_flat_large_ears),
            verify(is_dark_gray).




/* classification rules */
mammal    :- verify(has_hair), !.
mammal    :- verify(gives_milk).
bird      :- verify(has_feathers), !.
bird      :- verify(flys),
             verify(lays_eggs).
carnivore :- verify(eats_meat), !.
carnivore :- verify(has_pointed_teeth),
             verify(has_claws),
             verify(has_forward_eyes).
herbivorous :- verify(eat_grass),!.
herbivorous :- verify(has_flat_teeth),
               verify(has_small_claws),
               verify(has_forward_eyes).
ungulate :- mammal,
            verify(has_hooves), !.
ungulate :- mammal,
            verify(chews_cud).

/* how to ask questions */
ask(Question) :-
    nl,
    write('Does the animal have the following features: '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic yes/1,no/1.

/* How to verify something */
verify(S) :-
   (yes(S)
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

searching :-
    nl,write('Searching your animal....'),nl,
    animal(Animal),!,nl,
      write('Your Animal is recognize.\nYour animal is: '),
      write(Animal),undo.

searching :-
    nl,write('Sorry we could not recognize your animal!!'),nl,undo.

start:-
    nl,
    write("*************************************"),nl,
    write("*****Welcome to our Animo System*****"),nl,
    write("\t****Good Day****\n*************************************\n\nWe are going to ask you few questions?"),nl,nl,
    searching,nl,nl,
    tryagain,undo.

tryagain:-
    nl,
    write("Do you want to try again this System?(y/n)"),
    read(Response),
    ((Response==y;Response==yes)->
    start
    ;
    (Response==n;Response==no;Response==exit)->
      (write("Good Bye! Thanks for using this system"),nl,nl,undo)
    ;
    tryagain
    ).

