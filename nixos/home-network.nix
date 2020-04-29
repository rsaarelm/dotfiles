{ ... }:

{
  networking = {
    # DHCP server is configured to assign fixed IPs to the named home machines
    # based on MAC addresses.
    #
    # Schema for last octet of IP address:
    # Wired connection     = atomic number of name
    # Wireless connection  = atomic number of name + 100
    extraHosts = ''
      192.168.1.123 vanadium
      192.168.1.123 v

      192.168.1.173 tantalum
      192.168.1.173 ta

      192.168.1.74 tungsten
      192.168.1.74 w

      192.168.1.175 rhenium
      192.168.1.175 re
    '';
  };
}
