# icebox

I've been trying to measure my refrigerator and freezer temperatures
since a power outage a couple of months ago.  My zigbee idea worked
for around a day, then failed me miserably for reasons I don't fully
understand (other than the stack being somewhat unreliable).

I later learned about [AcuRite 986](https://www.amazon.com/dp/B004QJVU78)
kitchen thermometers that operate in the ISM band to populate a little
screen they come with.  [RTL 433](https://github.com/merbanan/rtl_433)
can pick this stuff up with cheap SDRs you probably have lying around,
so I grabbed a couple sets and tried it out.

One problem I (and apparently other people) ran into is that you can't
distinguish more than two units.  They're sold as a kit for your
kitchen and they assume everyone only has a refrigerator and a
freezer, so that's all you get.  The protocol communicates one of
two possible channel values and a semi-stable unit identifier (seems
to reset when you change batteries and possibly other times).

I also found that my radio picked up four plausible units that, if
they actually exist, are not mine.  (I live far away from anyone else
who'd be using these, but the problem might exist anyway).

So I built a little tool that will watch the MQTT data RTL_433 emits
and try to figure out which sensor is being reported and name it and
retransmit that name elsewhere.

## Prerequisites

First, get `rtl_433` doing the thing:

```sh
rtl_433 -F "mqtt://$mqttserver:1883,user=rtl433,retain=0,devices=tmp/rtl_433[/id]"
```

This causes messages to be delivered that look like this:

```
tmp/rtl_433/27397/time → 2020-11-06 19:03:40
tmp/rtl_433/27397/id → 27397
tmp/rtl_433/27397/channel → 1R
tmp/rtl_433/27397/battery_ok → 1
tmp/rtl_433/27397/temperature_F → 0
tmp/rtl_433/27397/status → 0
tmp/rtl_433/27397/mic → CRC
```

The fields are delivered as discrete messages, so I have to do a bit
of accumulation before I can transmit anything, but they should all
happen around the same time, so it's fine.

## Usage

Now that you've got data coming in, let's send it back out using this
tool:

```
Usage: icebox --mqttURI TEXT --inPrefix TEXT --outPrefix TEXT --nameFile STRING

Available options:
  -h,--help                Show this help text
  --mqttURI TEXT           URI of mqtt server to talk to
  --inPrefix TEXT          Input prefix (e.g., rtl_433/)
  --outPrefix TEXT         Output prefix (e.g., sensors/)
  --nameFile STRING        Path for channel naming rules file
```

You'll need to provide values for all the parameters, but `nameFile`
is probably the least obvious.  It provides default naming rules for
messages as they come in.  A super simple example might look liek
this:

```
channel 1 -> fridge
channel 2 -> freezer
```

That's enough to get you up and running with a pair of sensors.  But I
have three, two of which are on channel 1.  But I can tell them apart
by temperature, so before I just declare channel 1 fridge, I see if
it's too cold to be the fridge:

    channel = 1 && temperature < -5 -> deepfreeze

This line (above the other channel 1 definition) lets me tell my three
apart.  So starting with those rules, an invocation like this:

```
icebox --mqttURI mqtt://$mqttserver/ --inPrefix tmp/rtl_433/ --outPrefix sensors/ --nameFile name.conf
```

will get you results that look like this:

```
oro/rtl/freezer/temperature → -18.88888888888889
  MessageExpiryInterval 900
oro/rtl/freezer/battery_ok → 1
  MessageExpiryInterval 900
```

(i.e., the temperature and battery reading with a 15 minute
expiration)

This should also reject new devices coming in matching your current
rules, so my graphs will stop having weird spikes from rogue
readings.
