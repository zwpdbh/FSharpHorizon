namespace Others.Snippets 

module ResponseData = 
    let mapResponseStr = 
        """
        {
            "authenticationResultCode": "ValidCredentials",
            "brandLogoUri": "http://dev.virtualearth.net/Branding/logo_powered_by.png",
            "copyright": "Copyright ? 2023 Microsoft and its suppliers. All rights reserved. This API cannot be accessed and the content and any results may not be used, reproduced or transmitted in any manner without express written permission from Microsoft Corporation.",
            "resourceSets": [
                {
                    "estimatedTotal": 1,
                    "resources": [
                        {
                            "__type": "Location:http://schemas.microsoft.com/search/local/ws/rest/v1",
                            "bbox": [
                                39.817195892333984,
                                124.12994384765625,
                                40.156288146972656,
                                124.40470123291016
                            ],
                            "name": "Zhenxing District, China",
                            "point": {
                                "type": "Point",
                                "coordinates": [
                                    40.12990189,
                                    124.3833313
                                ]
                            },
                            "address": {
                                "adminDistrict": "Liaoning",
                                "adminDistrict2": "Dandong",
                                "countryRegion": "China",
                                "formattedAddress": "Zhenxing District, China",
                                "locality": "Zhenxing District"
                            },
                            "confidence": "High",
                            "entityType": "PopulatedPlace",
                            "geocodePoints": [
                                {
                                    "type": "Point",
                                    "coordinates": [
                                        40.12990189,
                                        124.3833313
                                    ],
                                    "calculationMethod": "Rooftop",
                                    "usageTypes": [
                                        "Display"
                                    ]
                                }
                            ],
                            "matchCodes": [
                                "Good"
                            ]
                        }
                    ]
                }
            ],
            "statusCode": 200,
            "statusDescription": "OK",
            "traceId": "65af3aeeb18d429abe886249bed877bd|CO00004BAE|0.0.0.1|Ref A: 047C4CDF114448729D5E2078D4C5735B Ref B: CO1EDGE1208 Ref C: 2023-03-06T13:43:55Z"
        }
        """

