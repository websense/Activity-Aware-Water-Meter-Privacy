# Activity-Aware-Water-Meter-Privacy
Code and sample data for "Activity-Aware Privacy Protection for Smart Water Meters" BuildSys 2021

Abstract: Identifying water end-uses from household meter readings yields valuable insights of commercial and environmental value for both customers and water providers. But smart water meter data may expose sensitive information about the activities of metered households.  This paper considers the case where a water provider wishes to publish a database of household water meter traces to be used by water analysts for research and planning purposes.   We consider the risks to privacy should an adversary gain access to this database, with the threat of uniquely identifying a household of interest and exposing their water use activities.  Previous work shows that the effectiveness of privacy protection techniques is strongly domain and application dependent,  but few privacy studies have considered smart water metering.  This paper introduces a framework for activity-aware privacy protection for databases of household smart water meters and evaluates its effectiveness using real-world and synthetic datasets.  We found that privacy-protection is strongly dependent on the type of activity.  Aggregating readings protects households from disclosure of small scale and common activities.  For individualistic, infrequent activities such as garden watering or clothes washing, we found that data-aware sampling of households and seasons offers the best protection.

Rachel Cardell-Oliver and Harrison Cater-Turner
Link to the paper: DOI 10.1145/3486611.3486650 
To be presented at [BuildSys 2021](https://buildsys.acm.org/2021/) in November.

## Bibtex
If you find this code or the paper useful, please consider citing:


    @inproceedings{cardell-oliver2021buildsys,
    title={Activity-Aware Privacy Protection for Smart Water Meters},
    author={Cardell-Oliver, R. and Carter-Turner, H.},
    booktitle={The 8th ACM International Conference on Systems for Energy-Efficient Buildings, Cities, and Transportation (BuildSys '21), November 17--18, 2021, Coimbra, Portugal},
    year={2021},
    url = {https://doi.org/10.1145/3486611.3486650},
    doi = {10.1145/3486611.3486650}
    }
