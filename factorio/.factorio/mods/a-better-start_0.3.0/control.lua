script.on_event(defines.events.on_player_created, function(event)
	local player = game.players[event.player_index]
	local inventories = {
		defines.inventory.player_armor,
		defines.inventory.player_main,
		defines.inventory.player_quickbar,
		defines.inventory.player_guns,
		defines.inventory.player_ammo,
		defines.inventory.player_tools,
		defines.inventory.player_vehicle,
		defines.inventory.player_player_trash
	}
	
	-- Setup kit presets
	local kits = {}
	kits["small"] = {}
	kits["small"]["quickbar"] = {
		{1, "transport-belt"},
		{2, "underground-belt"},
		{3, "splitter"}
	}
	kits["small"]["items"] = {
		{"steel-axe", 10},
		{"iron-plate", 192},
		{"copper-plate", 200},
		{"iron-gear-wheel", 50},
		{"transport-belt", 500},
		{"splitter", 50},
		{"underground-belt", 50},
		{"burner-mining-drill", 20},
		{"coal", 100}
	}
	
	kits["starter"] = {}
	kits["starter"]["quickbar"] = {
		{1, "transport-belt"},
		{2, "underground-belt"},
		{3, "splitter"},
		{4, "inserter"}
	}
	kits["starter"]["items"] = {
		{"steel-axe", 10},
		{"iron-plate", 292},
		{"copper-plate", 250},
		{"iron-gear-wheel", 50},
		{"transport-belt", 800},
		{"splitter", 50},
		{"underground-belt", 50},
		{"electric-mining-drill", 30},
		{"stone-furnace", 48},
		{"inserter", 100},
		{"coal", 100},
		{"boiler", 10},
		{"steam-engine", 20},
		{"offshore-pump", 1}
	}
	kits["starter"]["technologies"] = {
		{"automation"},
		{"electronics"},
		{"toolbelt"},
		{"logistics"},
		{"electric-energy-distribution-1"}
	}
		
	kits["medium"] = {}
	kits["medium"]["quickbar"] = {
		{1, "transport-belt"},
		{2, "underground-belt"},
		{3, "splitter"},
		{4, "inserter"},
		{5, "medium-electric-pole"},
		{6, "deconstruction-planner"},
		{10, "car"}
	}
	kits["medium"]["items"] = {
		{"steel-axe", 10},
		{"iron-plate", 592},
		{"copper-plate", 400},
		{"iron-gear-wheel", 200},
		{"electronic-circuit", 200},
		{"transport-belt", 1100},
		{"underground-belt", 50},
		{"splitter", 50},
		{"stone-furnace", 100},
		{"assembling-machine-1", 20},
		{"inserter", 300},
		{"long-handed-inserter", 50},
		{"steel-chest", 50},
		{"electric-mining-drill", 50},
		{"medium-electric-pole", 200},
		{"boiler", 10},
		{"steam-engine", 20},
		{"offshore-pump", 1},
		{"pipe-to-ground", 50},
		{"pipe", 50},
		{"car", 1},
		{"coal", 200},
		{"construction-robot", 50},
		{"lab", 10},
		{"deconstruction-planner", 1},
		{"power-armor", 1}
	}
	kits["medium"]["armorItems"] = {
		{"fusion-reactor-equipment"},
		{"personal-roboport-equipment"},
		{"personal-roboport-equipment"},
		{"personal-roboport-equipment"},
		{"personal-roboport-equipment"},
		{"personal-roboport-equipment"},
		{"battery-equipment"},
		{"battery-equipment"},
		{"battery-equipment"}
	}
	
	kits["medium"]["technologies"] = {
		{"automation"},
		{"electronics"},
		{"toolbelt"},
		{"logistics"},
		{"electric-energy-distribution-1"}
	}
	
	kits["big"] = {}
	kits["big"]["quickbar"] = {
		{1, "transport-belt"},
		{2, "underground-belt"},
		{3, "splitter"},
		{4, "inserter"},
		{5, "medium-electric-pole"},
		{6, "deconstruction-planner"},
		{10, "car"}
	}
	kits["big"]["items"] = {
		{"power-armor-mk2", 1},
		{"steel-axe", 10},
		{"iron-plate", 592},
		{"copper-plate", 400},
		{"iron-gear-wheel", 200},
		{"electronic-circuit", 200},
		{"advanced-circuit", 200},
		{"transport-belt", 1500},
		{"underground-belt", 50},
		{"splitter", 50},
		{"steel-furnace", 100},
		{"assembling-machine-2", 100},
		{"inserter", 300},
		{"long-handed-inserter", 50},
		{"steel-chest", 50},
		{"electric-mining-drill", 50},
		{"medium-electric-pole", 350},
		{"big-electric-pole", 100},
		{"logistic-chest-requester", 100},
		{"logistic-chest-passive-provider", 100},
		{"boiler", 20},
		{"steam-engine", 40},
		{"offshore-pump", 10},
		{"pipe-to-ground", 100},
		{"pipe", 100},
		{"chemical-plant", 20},
		{"oil-refinery", 10}, 
		{"car", 1},
		{"coal", 50},
		{"roboport", 20},
		{"construction-robot", 50},
		{"logistic-robot", 300},
		{"lab", 10},
		{"deconstruction-planner", 1},
		{"storage-tank", 10},
		{"logistic-chest-storage", 50},
	}
	
	kits["big"]["armorItems"] = {
		{"fusion-reactor-equipment"},
		{"fusion-reactor-equipment"},
		{"fusion-reactor-equipment"},
		{"exoskeleton-equipment"},
		{"exoskeleton-equipment"},
		{"exoskeleton-equipment"},
		{"exoskeleton-equipment"},
		{"energy-shield-mk2-equipment"},
		{"energy-shield-mk2-equipment"},
		{"personal-roboport-mk2-equipment"},
		{"night-vision-equipment"},
		{"battery-mk2-equipment"},
		{"battery-mk2-equipment"}
	}
	
	kits["big"]["technologies"] = {
		{"automation"},
		{"steel-processing"},
		{"automation-2"},
		{"oil-processing"},
		{"plastics"},
		{"advanced-electronics"},
		{"sulfur-processing"},
		{"battery"},
		{"toolbelt"},
		{"electronics"},
		{"engine"},
		{"electric-engine"},
		{"flying"},
		{"robotics"},
		{"logistic-robotics"},
		{"construction-robotics"},
		{"logistic-system"},
		{"fluid-handling"}
	}
	
		local kitSetting = settings.startup["a-better-start"].value
		local techSetting = settings.startup["a-better-start-technologies"].value
	local kit = kits[kitSetting]
	if kit == nil then
		kit = kits["medium"]
	end
	
	-- Find quickbar (usually player_quickbar, but god_quickbar in sandbox mode)
	local quickbar = player.get_inventory(defines.inventory.player_quickbar)
	if quickbar ~= nil and not quickbar.can_set_filter(1, "transport-belt") then
		quickbar = player.get_inventory(defines.inventory.god_quickbar)
	end
	
	-- Setup quickbar favorites
	if quickbar ~= nil and quickbar.can_set_filter(1, "transport-belt") ~= nil then
		quickbar.clear()
		for k,v in pairs(kit["quickbar"]) do
			quickbar.set_filter(v[1], v[2])
		end
	end
	
	-- Add items
	for k,v in pairs(kit["items"]) do
		player.insert{name = v[1], count = v[2]}
	end
	
	if kit["armorItems"] ~= nil then
		-- Find armor in one of the inventories
		-- Usually ends up in the armor slot. But that one does not exist in sandbox mode
		for k,v in pairs(inventories) do
			local inventory = player.get_inventory(v)
			if inventory ~= nil then
				local armor = inventory.find_item_stack("power-armor")
				if armor ~= nil then
					-- Add items to armor grid
					local grid = armor.grid
					for k,v in pairs(kit["armorItems"]) do
						grid.put{name = v[1]}
					end
					break
				end
			end
		end
	end
	-- YEA YEA I'M LAZY, THIS IS FOR MK2
	if kit["armorItems"] ~= nil then
		-- Find armor in one of the inventories
		-- Usually ends up in the armor slot. But that one does not exist in sandbox mode
		for k,v in pairs(inventories) do
			local inventory = player.get_inventory(v)
			if inventory ~= nil then
				local armor = inventory.find_item_stack("power-armor-mk2")
				if armor ~= nil then
					-- Add items to armor grid
					local grid = armor.grid
					for k,v in pairs(kit["armorItems"]) do
						grid.put{name = v[1]}
					end
					break
				end
			end
		end
	end
	
	
	-- Unlock 
	if techSetting then
	if kit["technologies"] ~= nil then
		for k,v in pairs(kit["technologies"]) do
			player.force.technologies[v[1]].researched = true
		end
	end
	end
	
end)