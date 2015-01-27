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
	
	int nbel <- 1;
	float share_prob <- 1.0;
	int viewbel <- 1;
	float mu <- 1.0;
	
	
	init {
		create roads from: roads_shapefile;
		road_network <- as_edge_graph(roads);
		create buildings from: buildings_shapefile {
			inhabitant_bel <- list_with(nbel,0.5);
		}
		create people number:2000 {
			buildings init_place <- one_of(buildings);
			location <- any_location_in(init_place) + {0,0, init_place.height};
			target <- any_location_in(one_of(buildings));

			home_bld <- one_of(buildings);
			home <- any_location_in(home_bld);
			
			bel <- list_with(nbel,float(rnd(100))/100);
			incert <- list_with(nbel,float(rnd(100))/100);
		}
	}
	
	
	
	
}

	
species people skills:[moving]{		
	float speed <- 5.0 + rnd(5);
	
	list<float> bel;
	list<float> incert;
	
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
		
		draw sphere(5) color: hsb(0.5-0.5*bel[viewbel-1],1,1);
	}
	
	action share_belief(people b){
		int bn <- rnd(nbel-1);
		float bi <- bel[bn];
		float bj <- b.bel[bn];
		float ui <- incert[bn];
		float uj <- b.incert[bn];
		
		float hij <- self.min(bi+ui,bj+uj) - self.max(bi-ui,bj-uj);
    
    
    	if(hij > ui){
    		b.bel[bn] <- bj + mu*(hij/ui - 1)*(bi-bj);
    		b.incert[bn] <-uj + mu*(hij/ui - 1)*(ui-uj); 
    	}
    	
    	if(hij > uj){
    		bel[bn] <- bi + mu*(hij/uj - 1)*(bj-bi);
    		incert[bn] <-ui + mu*(hij/uj - 1)*(uj-ui); 
    	}    	
    }
    
    float max (float i, float j) {
		return i > j ? i : j;
	}
	
	float min (float i, float j) {
		return i < j ? i : j;
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
	
	list<people> all_people update: self update_all_people ();
	float nbPeople update: float(length(all_people));
	
	float sumBelief update: update_sum_belief(all_people,0);
	float moyBelief update: (nbPeople>0)?sumBelief/nbPeople:0;
	
	float sumIncert;
	float moyIncert update: (nbPeople>0)?sumIncert/nbPeople:0;
	
	list<people> update_all_people{
		
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
		
		list<people> p <- [];
		add all: people to: p;
		ask buildings{
			add all: (self.members as list<people>) to: p;
		
		}
		
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
		display Charts {
			chart name: "Average of Beliefs" type: histogram background: rgb("lightGray") {
				data "Bel0" value: moyBelief color: rgb("red");
				data "Inc0" value: moyIncert color: rgb("green");
				//data "Bel1" value: sum(self.all_people collect(each.bel[1]))/(length(all_people)+0.000000001) color: rgb("green");

			}
		}
		
		display Map type: opengl ambient_light: 150{
			species roads aspect:geom;
			species buildings aspect:geom;
			species people aspect:circle;			
		}
	}
}