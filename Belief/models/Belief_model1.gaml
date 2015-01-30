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
	
	int nb_people <- 200 min : 1 step: 1 parameter: "Number of people" category: "Global parameters";
	int nbel <- 2 min : 1 step: 1 parameter: "Number of beliefs" category: "Global parameters";
	float share_prob <- 1.0;
	int viewbel <- 1 min : 1 step: 1 parameter: "Belief to display" category: "Display parameters";
	float mu <- 0.01 min: 0.0 max: 1.0 step: 0.01 parameter: "Speed of influence" category: "Global parameters";
	float min_incert <- 0.1 max: 1.0 min: 0.0 step: 0.1 parameter: "Minimum initial incertitude" category: "Global parameters";
	float max_incert <- 0.5 max: 1.0 min: 0.0 step: 0.1 parameter: "Maximum inital incertitude" category: "Global parameters";
	float family_influence_bel <- 0.2 min: 0.0 max: 1.0 step: 0.05 parameter: "Speed of influence of the family on own belief" category: "Global parameters";
	float family_influence_incert <- 0.01 min: 0.0 max: 0.1 step: 0.01 parameter: "Speed of influence of the family on own incertitude" category: "Global parameters";
	float home_influence <- 0.1 min: 0.0 max: 1.0 step: 0.05 parameter: "Speed of influence on the family" category: "Global parameters";
	
	list<float> moyBel <- [];
	list<float> moyInc <- [];
	list<int> nb_people_per_piece_of_pie <- [];
	int nb_piece_of_pie <- 7 min : 1 step: 1 parameter: "Number of piece of pie" category: "Display parameters";
	
	list<float> moyDist_to_home_bel <- [];
	
	init {
		create roads from: roads_shapefile;
		road_network <- as_edge_graph(roads);
		create buildings from: buildings_shapefile {
			inhabitant_bel <- list_with(nbel,rnd_float(1.0));
		}
		create people number:nb_people {
			buildings init_place <- one_of(buildings);
			location <- any_location_in(init_place) + {0,0, init_place.height};
			target <- any_location_in(one_of(buildings));

			home_bld <- one_of(buildings);
			home <- any_location_in(home_bld);
			
			bel <- list_with(nbel,0.0);
			incert <- list_with(nbel,0.0);
			loop i from: 0 to: nbel - 1 step:1 {
				bel[i] <- rnd_float(1.0);
				incert[i] <- min_incert+rnd_float(max_incert-min_incert);
			}
		}
		
		moyBel <- [];
		loop i from:0 to:nbel-1 {
			float bsum <- 0.0;
			ask people {
				bsum <- bsum + self.bel[i];
			}
			add bsum/nb_people to: moyBel;
		}
		
		moyInc <- [];
		loop i from:0 to:nbel-1 {
			float isum <- 0.0;
			ask people {
				isum <- isum + self.incert[i];
			}
			add isum/nb_people to: moyInc;
		}
		
		nb_people_per_piece_of_pie <- [];
		loop i from:0 to:nb_piece_of_pie-1 {
			add (people count ((each.bel[viewbel-1] >= i/nb_piece_of_pie) and (each.bel[viewbel-1] < ((i+1)/nb_piece_of_pie)+0.00000001))) to: nb_people_per_piece_of_pie;
		}
		
		moyDist_to_home_bel <- [];
		loop i from:0 to:nbel-1 {
			float dist_home_bel_sum <- 0.0;
			ask people {
				dist_home_bel_sum <- dist_home_bel_sum + abs(self.bel[i] - self.home_bld.inhabitant_bel[i]);
			}
			add dist_home_bel_sum/nb_people to: moyDist_to_home_bel;
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
			
			if(abs(dist)<incert[i]){
					/* influence of the families on the incertitude */
				incert[i] <- incert[i]+family_influence_incert*dist;
				if (incert[i] < min_incert) {
					incert[i] <- min_incert;
				}
				if (incert[i] > max_incert) {
				incert[i] <- max_incert;
				}
			
				/* influence of the families on the bielief directly */
				bel[i] <- bel[i]+family_influence_bel*dist;
				if (bel[i] < 0) {
					bel[i] <- 0;
				}
				if (bel[i] > 1) {
					bel[i] <- 1;
				}
				
				
			}
			
			/* influence of the inhabitants on their home */
			if(dist != 0) {
				home_bld.inhabitant_bel[i] <- bb-home_influence*dist;
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
		
		draw sphere(5) color: hsb(0.33*bel[viewbel-1],1,1);
	}
	
	action share_belief(people b){
		int bn <- rnd(nbel-1);
		people a <- self;
		float bi <- a.bel[bn];
		float bj <- b.bel[bn];
		float ui <- a.incert[bn];
		float uj <- b.incert[bn];
		
		float hij <- self.min(bi+ui,bj+uj) - self.max(bi-ui,bj-uj);
    
    
    	if(hij > uj){
  		  	loop i from: 0 to: length(a.bel) - 1 step:1 {
				a.bel[i] <-a.bel[i]+mu*(hij/uj - 1)*(b.bel[i]-a.bel[i]);
			}
    		//a.bel[bn] <- bi + mu*(hij/uj - 1)*(bj-bi);
    		a.incert[bn] <-ui + mu*(hij/uj - 1)*(uj-ui); 
    	}
    			
    	if(hij>ui){
    		loop i from: 0 to: length(b.bel) - 1 step:1 {
				b.bel[i] <-b.bel[i]+mu*(hij/ui - 1)*(a.bel[i]-b.bel[i]);
			}
    		//a.bel[bn] <- bi + mu*(hij/uj - 1)*(bj-bi);
    		b.incert[bn] <-uj + mu*(hij/ui - 1)*(ui-uj);    	
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
		draw shape color: hsb(0.33*inhabitant_bel[viewbel-1],1,1);
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
	list<people> all_people update: self update_all_people ();
	float nbPeople update: float(length(all_people));
	
	list<people> update_all_people{
		
		list<people> p <- [];
		add all: people to: p;
		ask buildings{
			add all: (self.members as list<people>) to: p;
		
		}
		
		/* update of global variables (maybe could be placed in a more appropriate position in the code) */
		if (!empty(p)) {
			/* update of moyBel */
			int len_p <- length(p);
			loop i from:0 to:nbel-1 {
				float bsum <- 0.0;
				ask p{
					bsum <- bsum + self.bel[i];
				}
				moyBel[i] <- bsum/len_p;
			}
			
			/* update of moyInc */
			loop i from:0 to:nbel-1 {
				float isum <- 0.0;
				ask p{
					isum <- isum + self.incert[i];
				}
				moyInc[i] <- isum/len_p;
			}
			
			/* update of nb_people_per_piece_of_pie */
			loop i from:0 to:nb_piece_of_pie-1 {
				nb_people_per_piece_of_pie[i] <- (p count ((each.bel[viewbel-1] >= i/nb_piece_of_pie) and (each.bel[viewbel-1] < ((i+1)/nb_piece_of_pie)+0.00000001)));
			}
			
			/*update of moyDist_to_home_bel */
			loop i from:0 to:nbel-1 {
				float dist_home_bel_sum <- 0.0;
				ask p {
					dist_home_bel_sum <- dist_home_bel_sum + abs(self.bel[i] - self.home_bld.inhabitant_bel[i]);
				}
				moyDist_to_home_bel[i] <- dist_home_bel_sum/len_p;
			}
		}
		
		return p;
	}
	
	output {
		display Belief {
			chart name: "Average of Beliefs" type: histogram background: rgb("lightGray") {
				/* Affichage des moyennes de chaque belief sur le même histogramme */
				loop i from: 0 to: nbel-1 {
					data "Bel_"+(i+1) value: moyBel[i] color: hsb(i/nbel,1,1);
				}
			}
		}
		
		display Incert {
			chart name: "Average of Beliefs" type: series background: rgb("lightGray") {
				/* Affichage des moyennes des incertitudes de chaque belief sur le même histogramme */
				loop i from: 0 to: nbel-1 {
					data "Inc_"+(i+1) value: moyInc[i] color: hsb(i/nbel,1,1) marker: false;
				}
			}
		}
		
		display Distri {
			chart name: "Distribution of the displayed Belief" type: pie background: rgb("lightGray") {
				loop i from:0 to:nb_piece_of_pie-1 {
					data ""+i+"/"+nb_piece_of_pie+" - "+(i+1)+"/"+nb_piece_of_pie value: nb_people_per_piece_of_pie[i] color: hsb(i/nb_piece_of_pie,1,1);
				}
			}
		}
		
		display Dist_Home {
			chart name: "Entropy of families' belief" type: series background: rgb("lightGray") {
				/* Affichage des moyennes des distances entre les belief de chaque habitant et de leur maisons */
				loop i from: 0 to: nbel-1 {
					data "Bel_"+(i+1) value: moyDist_to_home_bel[i] color: hsb(i/nbel,1,1) marker: false;
				}
			}
		}
		
		display Map type: opengl ambient_light: 150{
			species roads aspect:geom;
			species buildings aspect:geom;
			species people aspect:circle;
		}
	}
}