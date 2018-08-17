data:extend({
  {
    type = "string-setting",
    name = "a-better-start",
    order = "aa",
    setting_type = "startup",
    default_value = "starter",
    allowed_values =  {"small", "starter", "medium", "big"}
  },
  {
      type = "bool-setting",
      name = "a-better-start-technologies",
      setting_type = "startup",
      default_value = true,
   }
})