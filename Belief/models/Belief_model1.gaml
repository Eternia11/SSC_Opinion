/**
 * Ispired from PRIMA 2013, a toy model from GAMA
 * by REYMANN Christophe and BENIER Alan
 */
 
model Belief

global {
	file roads_shapefile <- file("../includes/road.shp");
	file buildings_shapefile <- file("../includes/building.shp");
	geometry shape <- envelope(roads_shapefile);
	graph road_network;
	
	float beta <- 0.4;
	int nbel <- 2;
	float share_prob <- 0.3;
	float attraction_threshold <- 0.2;
	int viewbel <- 1;
	
	init {
		create roads from: roads_shapefile;
		road_network <- as_edge_graph(roads);
		create buildings from: buildings_shapefile {
			inhabitant_bel <- list_with(nbel,0.5);
		}
		create people number:1000 {
			buildings init_place <- one_of(buildings);
			location <- any_location_in(init_place) + {0,0, init_place.height};
			target <- any_location_in(one_of(buildings));
			bel <- list_with(nbel,float(rnd(100))/100.0);
			home_bld <- one_of(buildings);
			home <- any_location_in(home_bld);
		}
	}
}

species people skills:[moving]{		
	float speed <- 5.0 + rnd(5);
	
	list<float> bel;
	point home;
	point target;
	
	buildings home_bld;
	
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
	
	reflex live_home when : (location = home) {
		loop i from: 0 to: nbel-1 { 
			float bb <- home_bld.inhabitant_bel[i];
			float bs <- bel[i];
			float dist <- bb-bs;
			if(dist != 0){
				home_bld.inhabitant_bel[i] <- bb-0.1*dist;
				if (home_bld.inhabitant_bel[i] < 0) {
					home_bld.inhabitant_bel[i] <- 0;
				}
				if (home_bld.inhabitant_bel[i] > 1) {
					home_bld.inhabitant_bel[i] <- 1;
				}
			}
		}
	}
	
	aspect circle {
		/* no elsewhere to put these control statements */
		if (nbel < 1) {
			nbel <- 1;
		}
		if (viewbel > nbel) {
			viewbel <- nbel;
		}
		if (viewbel < 1) {
			viewbel <- 1;
		}
		draw sphere(5) color: hsb(0.5-0.5*bel[viewbel-1],1,1);
	}
	
	action share_belief(people b){
		int bn <- rnd(nbel-1);
		float bb <- b.bel[bn];
		float bs <- bel[bn];
		float dist <- bb-bs;
		if(dist != 0){
			if(abs(dist)<attraction_threshold){
				bel[bn] 	<- bs+signum(dist)*0.1/(dist*dist);
				b.bel[bn] 	<- bb+signum(dist)*0.1/(dist*dist);
			}
			else{
				bel[bn] 	<- bs-signum(dist)*0.1*(dist*dist);
				b.bel[bn] 	<- bb+signum(dist)*0.1*(dist*dist);
			}
			if (bel[bn] < 0) {
				bel[bn] <- 0;
			}
			if (bel[bn] > 1) {
				bel[bn] <- 1;
			}
			if (b.bel[bn] < 0) {
				b.bel[bn] <- 0;
			}
			if (b.bel[bn] > 1) {
				b.bel[bn] <- 1;
			}
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
   	
   	list<float> inhabitant_bel;
   	
	aspect geom {
		draw shape color: hsb(0.5-0.5*inhabitant_bel[viewbel-1],1,1);
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
    	ask members as list<people>{
    		if(rnd_float(1.0)<share_prob){
    			people m <- one_of(myself.members) as people;
    			do share_belief(b: m);
    		}
    		
    	}
    }
    
  
}

experiment main_experiment type:gui{
	parameter 'Number of belief' var: nbel category: "Global parameter";
	parameter 'Belief to display' var: viewbel category: "Display parameter";
	
	output {
		display Charts {
			chart name: "Average of Beliefs" type: histogram background: rgb("lightGray") {
				data "Bel1" value: (sum (people  collect (each.bel[0])) + sum (buildings  collect sum(each.people_in_building collect (each.bel[0])))) / (length(people)+sum(buildings collect(length (each.members)))) color: rgb("green");
				data "bel2" value: (sum (people  collect (each.bel[1])) + sum (buildings  collect sum(each.people_in_building collect (each.bel[1])))) / (length(people)+sum(buildings collect(length (each.members)))) color: rgb("red");
			}
		}
		
		display Map type: opengl ambient_light: 150{
			species roads aspect:geom;
			species buildings aspect:geom;
			species people aspect:circle;			
		}
	}
}