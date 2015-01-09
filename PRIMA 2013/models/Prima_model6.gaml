/**
 *  model6
 *  This model illustrates EDO
 */ 
model model6 

global {
	file roads_shapefile <- file("../includes/road.shp");
	file buildings_shapefile <- file("../includes/building.shp");
	geometry shape <- envelope(roads_shapefile);
	graph road_network;
	
	float beta <- 0.4;
	int nbel <- 2;
	float share_prob <- 0.3;
	float attraction_threshold <- 0.2;
	
	init {
		create roads from: roads_shapefile;
		road_network <- as_edge_graph(roads);
		create buildings from: buildings_shapefile;
		create people number:1000 {
			buildings init_place <- one_of(buildings);
			location <- any_location_in(init_place) + {0,0, init_place.height};
			target <- any_location_in(one_of(buildings));
			bel <- list_with(nbel,float(rnd(100))/100.0);
			home <- any_location_in(one_of(buildings));
		}
	}
}

species people skills:[moving]{		
	float speed <- 5.0 + rnd(5);
	
	list<float> bel;
	point home;
	point target;
	reflex move {
		do goto target:target on: road_network;
		if (location = target) {
			if(target = home ){
				target <- any_location_in(one_of(buildings));
			}
			else{
				target <- home;
			}
		}
	}
	aspect circle{
		draw sphere(5) color: hsb(0.5-0.5*bel[0],1,1);
	}
	
	action share_belief(people b){
		int bn <- rnd(nbel-1);
		write("share belief bn "+bn);
		float bb <- b.bel[bn];
		float bs <- bel[bn];
		float dist <- bb-bs;
		if(dist=0){
			return 0;
		} 
		if(abs(dist)<attraction_threshold){
			bel[bn] 	<- bs+signum(dist)*0.1/(dist*dist);
			b.bel[bn] 	<- bb+signum(dist)*0.1/(dist*dist);
		}
		else{
			bel[bn] 	<- bs-signum(dist)*0.1*(dist*dist);
			b.bel[bn] 	<- bb+signum(dist)*0.1*(dist*dist);
		}
    }  
}

species roads {
	aspect geom {
		draw shape color: rgb("black");
	}
}

species buildings {
	float height <- 10.0+ rnd(10);
	int nbInhabitants update: length(members);				

	float t;    
   	float I_to_1 <- 0.0;
   	float h<-0.1;
   	
	aspect geom {
		draw shape color: empty(members) ? rgb("gray") : rgb("green") depth: height;
	}
	species people_in_building parent: people schedules: [] {
		int leaving_time;
		aspect circle{}
		
		reflex share_belief{}
	}
	reflex let_people_enter {
		list<people> entering_people <- (people inside self);
		if !(empty (entering_people)) {
			capture entering_people as: people_in_building returns: people_captured;
			ask people_captured {
				leaving_time <- int(time + 50 + rnd(50));
			}
 		}
	}
	reflex let_people_leave  {
		list<people_in_building> leaving_people <- list<people_in_building>(members) where (time >= each.leaving_time);
		if !(empty (leaving_people)) {
			release leaving_people as: people in: world;
		}
	}

	reflex share_bel when:length(members)>1{ 	
    	write("share_bel "+length(members));
    	ask members as list<people>{
    		write("share_bel2");
    		if(rnd_float(1.0)<share_prob){
    			people m <- one_of(myself.members) as people;
    			write(""+self+"share with "+m);
    			do share_belief(b: m);
    		}
    		
    	}
    }
    
  
}

experiment main_experiment type:gui{
	output {
		display map type: opengl ambient_light: 150{
			species roads aspect:geom;
			species buildings aspect:geom;
			species people aspect:circle;			
		}
	}
}