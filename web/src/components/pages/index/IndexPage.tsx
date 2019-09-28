import * as React from "react";
import { Button, Card, Col, Jumbotron, Media, Row } from "react-bootstrap";
import { Link } from "react-router-dom";
import BaseContainerPage from "../BaseContainerPage";
import { PresentationCard } from "./PresentationCard";

export interface IIndexPageProps {}

export default function IndexPage(props: IIndexPageProps) {
  return (
    <BaseContainerPage
      title="Index Page"
      lead="Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et 
      dolore magna aliqua."
    >
      <p>
        Ipsum consequat nisl vel pretium lectus. Scelerisque mauris pellentesque pulvinar pellentesque habitant morbi.
        Amet purus gravida quis blandit turpis cursus in. Pellentesque habitant morbi tristique senectus et netus et
        malesuada fames. Condimentum mattis pellentesque id nibh.
      </p>

      <Row>
        <PresentationCard 
          title="Advanced Stats"
          icon={<span className="glyphicon glyphicon-stats"></span>}
          content="We provide detailed statistics data in our dashboard page."
        />
        <PresentationCard
          title="Exportable Data"
          icon={<span className="glyphicon glyphicon-cloud-download"></span>}
          content="You can conveniently export all the data you see."
        />
        <PresentationCard
          title="Carbon-neutral"
          icon={<span className="glyphicon glyphicon-tree-deciduous"></span>}
          content="We compensate every tree that was cut down because of our business."
        />
        <PresentationCard
          title="Highly configurable"
          icon={<span className="glyphicon glyphicon-cog"></span>}
          content="We try not to provide opinionated decisions and be as flexible as possible."
        />
      </Row>

      <Jumbotron>
        <h2>Sweet cookies!</h2>
        <p className="lead">Every friday id velit ut tortor pretium viverra suspendisse potenti nullam.</p>
        <hr className="my-3" />
        <p>
          Risus in hendrerit gravida rutrum quisque. Tellus molestie nunc non blandit. Interdum velit laoreet id donec.
          Mus mauris vitae ultricies leo. Tincidunt augue interdum velit euismod in pellentesque massa placerat duis.
        </p>
        <Button as={Link} to="/offers/cookies">
          Learn More
        </Button>
      </Jumbotron>

      <Row>
        <Col>
          <Media as="li">
            <img src="https://via.placeholder.com/100" className="mr-3 rounded-circle" alt="placeholder"></img>
            <Media.Body>
              <h5>Lorem ipsum dolor sit amet!</h5>
              Ornare lectus sit amet est. Purus semper eget duis at tellus at urna. Id eu nisl nunc mi ipsum faucibus
              vitae. Consectetur adipiscing elit ut aliquam purus sit. Sit amet aliquam id diam maecenas ultricies mi
              eget. Cursus metus aliquam eleifend mi in. Eu nisl nunc mi ipsum. Urna et pharetra pharetra massa massa
              ultricies mi. Ipsum consequat nisl vel pretium lectus. Scelerisque mauris pellentesque pulvinar
              pellentesque habitant morbi. Amet purus gravida quis blandit turpis cursus in. Pellentesque habitant morbi
              tristique senectus et netus et malesuada fames. Condimentum mattis pellentesque id nibh.
            </Media.Body>
          </Media>
          <Media as="li" className="mt-5">
            <img src="https://via.placeholder.com/100" className="mr-3 rounded-circle" alt="another placeholder"></img>
            <Media.Body>
              <h5>Elementum tempus egestas sed sed risus</h5>
              Enim nulla aliquet porttitor lacus luctus. Montes nascetur ridiculus mus mauris vitae ultricies leo
              integer. Amet massa vitae tortor condimentum lacinia quis vel eros donec. Nibh nisl condimentum id
              venenatis a condimentum vitae sapien. Mattis nunc sed blandit libero volutpat. Vulputate dignissim
              suspendisse in est.
            </Media.Body>
          </Media>
        </Col>
        <Col xs={12} md={4} className="mt-sm-5 mt-5 mt-md-0">
          <Card className="shadow">
            <Card.Header>Featured</Card.Header>
            <Card.Body>
              <Card.Title>Special offer!</Card.Title>
              <Card.Text>Visit this thing and get a discount.</Card.Text>
              <Button className="px-5">Visit</Button>
            </Card.Body>
          </Card>
        </Col>
      </Row>

      <p className="mt-5">
        Suspendisse in est ante in nibh mauris cursus. Sed turpis tincidunt id aliquet risus feugiat in ante metus.
        Integer eget aliquet nibh praesent tristique magna sit. Eleifend donec pretium vulputate sapien nec. Tempus urna
        et pharetra pharetra massa. Ut aliquam purus sit amet. Orci dapibus ultrices in iaculis nunc sed augue lacus
        viverra. Fames ac turpis egestas maecenas pharetra convallis posuere morbi. Lacus viverra vitae congue eu
        consequat ac felis donec et. Viverra suspendisse potenti nullam ac. Vel fringilla est ullamcorper eget nulla
        facilisi etiam dignissim diam. Sagittis id consectetur purus ut. Hac habitasse platea dictumst vestibulum. Enim
        ut sem viverra aliquet eget sit amet tellus. Auctor urna nunc id cursus metus aliquam eleifend mi in. Amet
        consectetur adipiscing elit ut aliquam purus. At elementum eu facilisis sed odio. Enim ut sem viverra aliquet
        eget sit amet tellus cras.
      </p>
    </BaseContainerPage>
  );
}
