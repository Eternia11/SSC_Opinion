/**
 *  Belief Grid
 *  Author: christophe
 *  Description: 
 */

model BeliefGrid


global {
	
	float mu <- 0.1;
	int nbel <- 1;
	int viewbel <- 1;

	const black type: rgb <- rgb ("black");
	float density_of_people <- 0.7 parameter: "Density of people:" category: "Population" min: 0.01 max: 1.0;
	int dimensions <- 40 max: 400 min: 10 parameter: "Width and height of the environment:" category: "Environment";
	int neighbours_distance <- 1 max: 10 min: 1 parameter: "Distance of perception:" category: "Population";
	int number_of_people <- 0;
	//int sum_total_neighbours <- 1 update: sum (all_people collect each.total_nearby) min: 1;
	list<base> all_people <- [];  
	action description {
		write
		"\\n\\u25B6 Description. \\n\\u25B6 Thomas Schelling model of residential segregation is a classic study of the effects of local decisions on global dynamics. Agents with mild preferences for same-type neighbors, but without preferences for segregated neighborhoods, can wind up producing complete segregation.\\n\\u25B6 In this model, agents populate a grid with a given *density*. They are in two different states : happy when the percentage of same-color neighbours is above their *desired percentage of similarity*; unhappy otherwise. In the latter case, they change their location randomly until they find a neighbourhood that fits their desire. \\n\\u25B6 In addition to the previous parameter, one can adjust the *distance of perception* (i.e.  the distance at which they consider other agents as neighbours) of the agents to see how it affects the global process. ";
	}

	init { 
		do description;
		do initialize_places;
		number_of_people <- int( length (all_places) * density_of_people);
		do initialize_people;
	}

	list<space> free_places <- [] ;
	list<space> all_places <- [] ;
	geometry shape <- square(dimensions);
	
	action initialize_people { 
		create people number: number_of_people; 
		all_people <- people as list ;
	} 
	action initialize_places { 
		all_places <- shuffle (space);
		free_places <- all_places;  
	} 
	
}

entities {
	
}


global {
	
}
entities {
	grid space width: dimensions height: dimensions neighbours: 8 use_regular_agents: false frequency: 0{
		const color type: rgb <- black;
	}
	
	species base {
		list<base> my_neighbours;

		int total_nearby -> {
			length (my_neighbours)
		};
	}

	species people parent: base  {
		list<people> my_neighbours;
		space my_place;
		
			list<float> bel;
			list<float> incert;
	
		init {
			my_place <- one_of(free_places);
			location <- my_place.location; 
			remove my_place from: free_places;
			bel <- list_with(nbel,float(rnd(100))/100);
			incert <- list_with(nbel,float(rnd(100))/100);
			people temp <- self;
			ask people at_distance neighbours_distance{
				add temp to:my_neighbours;
			}
			add all:(people at_distance neighbours_distance) to:my_neighbours;
			
		} 
		reflex share_belief{
			people a <- self;
			int bn <- rnd(nbel-1);
			
			ask my_neighbours{
				float bi <- a.bel[bn];		
				float ui <- a.incert[bn];
			
				people b <- self;
				float bj <- b.bel[bn];
				float uj <- b.incert[bn];
			
		
				float hij <- self.min(bi+ui,bj+uj) - self.max(bi-ui,bj-uj);
    

    	
  		  		if(hij > uj){
    				a.bel[bn] <- bi + mu*(hij/uj - 1)*(bj-bi);
    				a.incert[bn] <-ui + mu*(hij/uj - 1)*(uj-ui); 
    			}
			}
			
		}
		
		float max (float i, float j) {
			return i > j ? i : j;
		}
	
		float min (float i, float j) {
			return i < j ? i : j;
		}  
		
		aspect default{ 
			draw sphere(0.5) color: hsb(0.5-0.5*bel[viewbel-1],1,1);
		}
	}
}


experiment beliefgrid type: gui {	
	output {
		display BeliefGrid {
			species people;
		}	
		display Charts {
			chart name: "Proportion of happiness" type: pie background: rgb("gray") style: exploded position: {0,0} size: {1.0,0.5}{
				//data "Unhappy" value: number_of_people - sum_happy_people color: rgb("green");
				//data "Happy" value: sum_happy_people color: rgb("yellow");
			}
			chart name: "Global happiness and similarity" type: series background: rgb("gray") axes: rgb("white") position: {0,0.5} size: {1.0,0.5} {
				//data "happy" color: rgb("blue") value:  (sum_happy_people / number_of_people) * 100 style: spline ;
				//data "similarity" color: rgb("red") value:  (sum_similar_neighbours / sum_total_neighbours) * 100 style: step ;
			}
		}
	}
}

