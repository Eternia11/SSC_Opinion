/**
 *  Belief Grid
 *  Author: christophe
 *  Description: 
 */

model BeliefGrid


global skills:[graphic]{
	
	float mu <- 0.01 min: 0.0 max: 1.0 parameter: "Speed of influence:" category: "Population";
	int nbel <- 1 min : 1 max : 2 parameter: "Number of beliefs:" category:"Population";
	int viewbel <- 1;
	int viewbel2 <- (nbel=2)?2:1;
	const black type: rgb <- rgb ("black");
	float density_of_people <- 1.0 parameter: "Density of people:" category: "Population" min: 0.01 max: 1.0;
	int dimensions <- 5 max: 400 min: 2 parameter: "Width and height of the environment:" category: "Environment";
	int neighbours_distance <- 1 max: 10 min: 1 parameter: "Distance of perception:" category: "Population";
	float min_incert <- 0.1 max: 1.0 min: 0.0 parameter: "Minimum initial incertitude:" category: "Population";
	float max_incert <- 0.5 max: 1.0 min: 0.0 parameter: "Maximum inital incertitude:" category: "Population";
	float def_rej <- 0.5 max: 1.0 min: 0.0 parameter: "Rejectance" category: "Population";
	
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
	grid space width: dimensions height: dimensions neighbours: 8 use_regular_agents: false frequency: 0{
		const color type: rgb <- black;
	}
	
	species base {
		list<base> my_neighbours;

		int total_nearby -> {
			length (my_neighbours)
		};
	}

	species people parent: base  skills:[graphic]{
		list<people> my_neighbours -> {people at_distance neighbours_distance};
		space my_place;
		
			list<float> bel;
			list<float> incert;
			list<float> rej;
	
		float viewbel_val -> {bel[viewbel-1]};
		init {
			my_place <- one_of(free_places);
			location <- my_place.location; 
			remove my_place from: free_places;
			bel <- list_with(nbel,0.0);
			incert <- list_with(nbel,0.0);
			rej <- list_with(nbel,0.0);
			loop i from: 0 to: nbel - 1 step:1 {
				bel[i] <- rnd_float(1.0);
				incert[i] <- min_incert+rnd_float(max_incert-min_incert);
				rej[i] <- def_rej; 
				
			}
			
			
		} 
		reflex share_belief{
			people a <- self;
			int bn <- rnd(nbel-1);
			
			ask one_of(my_neighbours){
				float bi <- a.bel[bn];		
				float ui <- a.incert[bn];
				float ri <- a.rej[bn];
				
				people b <- self;
				float bj <- b.bel[bn];
				float uj <- b.incert[bn];
				float rj <- b.rej[bn];
				
			
		
				//float hij <- self.min(bi+ui,bj+uj) - self.max(bi-ui,bj-uj);
    

    	
  		  		if(abs(bj-bi)<ui){
  		  			loop i from: 0 to: length(a.bel) - 1 step:1 {
						a.bel[i] <- max(0.0,min(1.0,a.bel[i]+mu*(b.bel[i]-a.bel[i])));
					}
				}else if(abs(bj-bi)>rj){
					loop i from: 0 to: length(b.bel) - 1 step:1 {
						a.bel[i] <- max(0.0,min(1.0,a.bel[i]-mu*(b.bel[i]-a.bel[i])));
					}
				}
    			
    			
    			
    			if(abs(bj-bi)<uj){
    				loop i from: 0 to: length(b.bel) - 1 step:1 {
						b.bel[i] <- max(0.0,min(1.0,b.bel[i]+mu*(a.bel[i]-b.bel[i])));
					}
				}else if(abs(bj-bi)>rj){
					loop i from: 0 to: length(b.bel) - 1 step:1 {
						b.bel[i] <- max(0.0,min(1.0,b.bel[i]-mu*(a.bel[i]-b.bel[i])));
					}
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
			draw square(1) color: hsb(0.33*bel[viewbel-1],1,1);

		}
		aspect default2{ 
			draw square(1) color: hsb(0.33*bel[viewbel2-1],1,1);

		}
	}
}


experiment beliefgrid type: gui {	
	
	list<people> all_people update: self update_all_people ();
	float nbPeople update: float(length(all_people));
	
	float sumBelief update: update_sum_belief(all_people,0);
	float moyBelief update: (nbPeople>0)?sumBelief/nbPeople:0;
	
	float sumIncert;
	float moyIncert update: (nbPeople>0)?sumIncert/nbPeople:0;
	
	list<people> update_all_people{
		
		list<people> p <- [];
		add all: people to: p;
		
		return p;
	}
	
	float update_sum_belief(list<people> p,int num){
		float bsum <- 0.0;
		float isum <- 0.0;
		ask p{
			bsum <- bsum + self.bel[num];
			isum <- isum + self.incert[num];
		}
		
		sumIncert <- isum;
		return bsum;	
	}
	
	output {
		display BeliefGrid {
			species people aspect:default ;
			
		}
		display BeliefGrid2{
						species people aspect:default2 ;
		}		
		display Charts refresh_every: 1{
			chart name: "Proportion of happiness" type: series background: rgb("white") style: exploded position: {0,0} size: {1.0,0.5}{
				data "]0;0.25]" value: people count (each.viewbel_val <= 0.25) color:#blue;
				data "]0.25;0.5]" value: people count ((each.viewbel_val > 0.25) and (each.viewbel_val <= 0.5)) color:#blue;
				data "]0.5;0.75]" value: people count ((each.viewbel_val > 0.5) and (each.viewbel_val <= 0.75)) color:#blue;
				data "]0.75;1]" value: people count (each.viewbel_val > 0.75) color:#blue;

				//data "Unhappy" value: number_of_people - sum_happy_people color: rgb("green");
				//data "Happy" value: sum_happy_people color: rgb("yellow");
			}
			chart name: "Global happiness and similarity" type: series background: rgb("gray") axes: rgb("white") position: {0,0.5} size: {1.0,0.5} {
				//data "happy" color: rgb("blue") value:  (sum_happy_people / number_of_people) * 100 style: spline ;
				//data "similarity" color: rgb("red") value:  (sum_similar_neighbours / sum_total_neighbours) * 100 style: step ;
				data "Bel0" value: moyBelief color: rgb("red");
				data "Inc0" value: moyIncert color: rgb("green");
			}
		}
	}
}

